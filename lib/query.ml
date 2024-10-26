open Lwt.Infix
module Worker = Ocaml_ci_api.Worker

let ( >>!= ) = Lwt_result.bind
let id = "opam-vars"

type t = { conn : Current_ocluster.Connection.t }

module Key = struct
  type t = { variant : Variant.t; lower_bound : bool; pool : string }
  [@@deriving to_yojson]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = struct
  type t = { image : string } [@@deriving to_yojson]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Outcome = struct
  type t = { image : string; vars : Worker.Vars.t } [@@deriving yojson]

  let marshal t = Yojson.Safe.to_string (to_yojson t)

  let unmarshal s =
    match of_yojson (Yojson.Safe.from_string s) with
    | Ok x -> x
    | Error e -> failwith e
end

(* This is needed iff the opam used isn't the image default opam. *)
let prepare_image ~variant =
  let opam = "opam-" ^ Opam_version.to_string (Variant.opam_version variant) in
  let prefix =
    match Variant.os variant with
    | `macOS -> "~/local"
    | `windows | `linux -> "/usr"
    | `freeBSD -> "/usr/local"
  in
  let ln =
    match Variant.os variant with
    | `windows | `macOS -> "ln"
    | `linux | `freeBSD -> "sudo ln"
  in
  (* XXX: don't overwrite default config? *)
  let opamrc = "" in
  let open Obuilder_spec in
  [
    run "%s -f %s/bin/%s %s/bin/opam" ln prefix opam prefix;
    run "opam init --reinit%s -ni" opamrc;
  ]

let opam_template arch =
  let arch = Option.value ~default:"%{arch}%" arch in
  Fmt.str
    {|
  {
    "arch": "%s",
    "os": "%%{os}%%",
    "os_family": "%%{os-family}%%",
    "os_distribution": "%%{os-distribution}%%",
    "os_version": "%%{os-version}%%",
    "opam_version": "%%{opam-version}%%"
  }
|}
    arch

let tail ~buffer ~job build_job =
  let rec aux start =
    Cluster_api.Job.log build_job start >>= function
    | Error (`Capnp e) -> Lwt.return @@ Fmt.error_msg "%a" Capnp_rpc.Error.pp e
    | Ok ("", _) -> Lwt_result.return ()
    | Ok (data, next) ->
        Buffer.add_string buffer data;
        Current.Job.write job data;
        aux next
  in
  aux 0L

let run_job ~buffer ~job build_job =
  let on_cancel _ =
    Cluster_api.Job.cancel build_job >|= function
    | Ok () -> ()
    | Error (`Capnp e) ->
        Current.Job.log job "Cancel failed: %a" Capnp_rpc.Error.pp e
  in
  Current.Job.with_handler job ~on_cancel @@ fun () ->
  let result = Cluster_api.Job.result build_job in
  tail ~buffer ~job build_job >>!= fun () ->
  result >>= function
  | Error (`Capnp e) ->
      Lwt_result.fail (`Msg (Fmt.to_to_string Capnp_rpc.Error.pp e))
  | Ok _ as x -> Lwt.return x

let parse_output job build_job =
  let buffer = Buffer.create 1024 in
  Capnp_rpc_lwt.Capability.with_ref build_job (run_job ~buffer ~job)
  >>!= fun (_ : string) ->
  match Astring.String.cuts ~sep:"\n@@@OUTPUT\n" (Buffer.contents buffer) with
  | [ _; output; _; output2; _ ] ->
      Current.Job.log job "@[<v2>Result:@,%s,%s@]" output output2;
      Lwt_result.return (Some (output, output2))
  | [ _; rest ] when Astring.String.is_prefix ~affix:"@@@OUTPUT\n" rest ->
      Lwt_result.return None
  | _ -> Lwt_result.fail (`Msg "Missing output from command\n")

let set_personality ~variant =
  if Variant.arch variant |> Ocaml_version.arch_is_32bit then
    [ Obuilder_spec.shell [ "/usr/bin/linux32"; "/bin/sh"; "-c" ] ]
  else []

let run { conn } job { Key.variant; lower_bound; pool } { Value.image } =
  let open Obuilder_spec in
  let arch =
    Variant.arch variant |> fun v ->
    if Ocaml_version.arch_is_32bit v then Some (Ocaml_version.to_opam_arch v)
    else None
  in
  let spec =
    Obuilder_spec.stage ~from:image
      ((user_unix ~uid:1000 ~gid:1000 :: set_personality ~variant)
      @ prepare_image ~variant
      @ [
          run
            "echo '@@@OUTPUT' && opam list -s --color=never --installed \
             ocaml-base-compiler ocaml-variants ocaml-system --column package \
             && echo '@@@OUTPUT'";
          run "echo '@@@OUTPUT' && opam config expand '%s' && echo '@@@OUTPUT'"
            (opam_template arch);
        ])
  in
  let spec_str = Fmt.to_to_string Obuilder_spec.pp spec in
  let action = Cluster_api.Submission.obuilder_build spec_str in
  let pool =
    Current_ocluster.Connection.pool ~job ~pool
      ~cache_hint:("opam-vars-" ^ image) ~action conn
  in
  Current.Job.start_with ~pool job ~level:Current.Level.Mostly_harmless
  >>= parse_output job
  >>!= fun s ->
  let ocaml_package, vars =
    match s with Some (a, b) -> (a, b) | None -> assert false
  in
  let ocaml_package = String.trim ocaml_package in
  let ocaml_package_name, ocaml_version =
    match Astring.String.cut ~sep:"." ocaml_package with
    | Some (name, version) -> (name, version)
    | None -> Fmt.failwith "Unexpected opam package name: %s" ocaml_package
  in
  let json =
    match Yojson.Safe.from_string vars with
    | `Assoc items ->
        `Assoc
          (("ocaml_package", `String ocaml_package_name)
          :: ("ocaml_version", `String ocaml_version)
          :: ("lower_bound", `Bool lower_bound)
          :: items)
    | json ->
        Fmt.failwith "Unexpected JSON: %a"
          Yojson.Safe.(pretty_print ~std:true)
          json
  in
  Current.Job.log job "@[<v2>Result:@,%a@]"
    Yojson.Safe.(pretty_print ~std:true)
    json;
  match Worker.Vars.of_yojson json with
  | Error msg -> Lwt_result.fail (`Msg msg)
  | Ok vars -> Lwt_result.return { Outcome.vars; image }

let pp f (key, value) =
  Fmt.pf f "opam vars of %a@,(%s)" Variant.pp key.Key.variant value.Value.image

let auto_cancel = false
let latched = true
