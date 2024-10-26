open Lwt.Infix
module Raw = Current_docker.Raw
module Worker = Ocaml_ci_api.Worker

let ( >>!= ) = Lwt_result.bind
let id = "opam-vars-local"

type t = { pool : unit Current.Pool.t }

module Key = struct
  type t = {
    docker_context : string option;
    variant : Variant.t;
    lower_bound : bool;
  }
  [@@deriving to_yojson]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = struct
  type t = { image : string; host_image : string } [@@deriving to_yojson]

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
let prepare_image ~job ~docker_context ~tag variant image =
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
  let spec =
    let open Obuilder_spec in
    stage ~from:image
      [
        run "%s -f %s/bin/%s %s/bin/opam" ln prefix opam prefix;
        run "opam init --reinit%s -ni" opamrc;
      ]
    |> Docker.dockerfile_of_spec ~buildkit:true ~os:`Unix
  in
  let cmd =
    Raw.Cmd.docker ~docker_context [ "build"; "--pull"; "-t"; tag; "-" ]
  in
  Current.Process.exec ~stdin:spec ~cancellable:false ~job cmd >>!= fun () ->
  Lwt_result.ok (Lwt.return tag)

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

let get_vars ~arch ~docker_context image =
  Raw.Cmd.docker ~docker_context
    [ "run"; "-i"; image; "opam"; "config"; "expand"; opam_template arch ]

let get_ocaml_package ~docker_context image =
  Raw.Cmd.docker ~docker_context
    [
      "run";
      "-i";
      image;
      "opam";
      "list";
      "-s";
      "--color=never";
      "--installed";
      "ocaml-base-compiler";
      "ocaml-variants";
      "ocaml-system";
      "--column";
      "package";
    ]

let run { pool } job { Key.docker_context; variant; lower_bound }
    { Value.image; host_image } =
  Current.Job.start job ~pool ~level:Current.Level.Mostly_harmless >>= fun () ->
  let prep_image =
    Fmt.str "ocurrent/ocaml-ci:%s" (Variant.docker_tag variant)
  in
  prepare_image ~job ~docker_context ~tag:prep_image variant host_image
  >>!= fun host_image ->
  let cmd = get_ocaml_package ~docker_context host_image in
  Current.Process.check_output ~cancellable:false ~job cmd
  >>!= fun ocaml_package ->
  let ocaml_package = String.trim ocaml_package in
  let ocaml_package_name, ocaml_version =
    match Astring.String.cut ~sep:"." ocaml_package with
    | Some (name, version) -> (name, version)
    | None -> Fmt.failwith "Unexpected opam package name: %s" ocaml_package
  in
  let arch =
    Variant.arch variant |> fun v ->
    if Ocaml_version.arch_is_32bit v then Some (Ocaml_version.to_opam_arch v)
    else None
  in
  let cmd = get_vars ~arch ~docker_context host_image in
  Current.Process.check_output ~cancellable:false ~job cmd >>!= fun vars ->
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
