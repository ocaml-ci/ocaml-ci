open Current.Syntax
module Raw = Current_docker.Raw
module Worker = Ocaml_ci_api.Worker

(* OCluster pool name *)
module Pool_name = struct
  type t =
    [ `Linux_x86_64
    | `Linux_ARM64
    | `Linux_ppc64
    | `Linux_s390x
    | `Linux_riscv64
    | `Macos_x86_64
    | `Macos_ARM64
    | `Windows_x86_64
    | `FreeBSD_x86_64 ]

  let to_string = function
    | `Linux_x86_64 -> "linux-x86_64"
    | `Linux_ARM64 -> "linux-arm64"
    | `Linux_ppc64 -> "linux-ppc64"
    | `Linux_s390x -> "linux-s390x"
    | `Linux_riscv64 -> "linux-riscv64"
    | `Macos_x86_64 -> "macos-x86_64"
    | `Macos_ARM64 -> "macos-arm64"
    | `Windows_x86_64 -> "test"
    | `FreeBSD_x86_64 -> "freebsd-x86_64"

  let of_string = function
    | "linux-x86_64" -> Ok `Linux_x86_64
    | "linux-arm64" -> Ok `Linux_ARM64
    | "linux-ppc64" -> Ok `Linux_ppc64
    | "linux-s390x" -> Ok `Linux_s390x
    | "linux-riscv64" -> Ok `Linux_riscv64
    | "macos-x86_64" -> Ok `Macos_x86_64
    | "macos-arm64" -> Ok `Macos_ARM64
    | "test" -> Ok `Windows_x86_64
    | "freebsd-x86_64 " -> Ok `FreeBSD_x86_64
    | s -> Error (`Msg (s ^ ": invalid pool name"))
end

type t = {
  label : string;
  builder : Builder.t;
  pool : Pool_name.t; (* OCluster pool *)
  variant : Variant.t;
  base : Current_docker.Raw.Image.t; (* Base image to use *)
  vars : Worker.Vars.t;
}

let pp f t = Fmt.string f t.label
let compare a b = compare a.label b.label

let compiler_matches_major_and_minor vars ~version =
  let vars_version =
    Ocaml_version.with_just_major_and_minor
      (Ocaml_version.of_string_exn vars.Worker.Vars.ocaml_version)
  in
  Ocaml_version.equal vars_version
    (Ocaml_version.with_just_major_and_minor version)

let set_compiler_version vars ~version =
  let ocaml_version = Ocaml_version.to_string version in
  { vars with Worker.Vars.ocaml_version }

module QC = Current_cache.Generic (Query)

let query conn ~variant ~lower_bound ~pool image =
  let label = if lower_bound then "opam-vars (lower-bound)" else "opam-vars" in
  Current.component "%s" label
  |> let> image in
     QC.run { conn }
       { Query.Key.variant; lower_bound; pool }
       { Query.Value.image }

module QCL = Current_cache.Generic (Query_local)

let query_local builder ~variant ~lower_bound ~host_image image =
  let label = if lower_bound then "opam-vars (lower-bound)" else "opam-vars" in
  Current.component "%s" label
  |> let> host_image and> image in
     let image = Raw.Image.hash image in
     let host_image = Raw.Image.hash host_image in
     let docker_context = builder.Builder.docker_context in
     let pool = builder.Builder.pool in
     QCL.run { pool }
       { Query_local.Key.docker_context; variant; lower_bound }
       { Query_local.Value.image; host_image }

let get_docker conn builder variant ~lower_bound tag label pool =
  let+ { Query.Outcome.vars; image } =
    query conn ~variant ~lower_bound ~pool:(Pool_name.to_string pool) tag
  in
  let base = Raw.Image.of_hash image in
  { label; builder; pool; variant; base; vars }

let get_docker_local builder variant ~lower_bound host_base base label pool =
  let+ { Query_local.Outcome.vars; image } =
    query_local builder ~variant ~lower_bound ~host_image:host_base base
  in
  let base = Raw.Image.of_hash image in
  { label; builder; pool; variant; base; vars }

let get ~arch ~label ~conn ~builder ~pool ~distro ~ocaml_version ~opam_version
    ~lower_bound tag =
  match Variant.v ~arch ~distro ~ocaml_version ~opam_version with
  | Error (`Msg m) -> Current.fail m
  | Ok variant ->
      let upper_bound =
        get_docker conn builder variant ~lower_bound:false tag label pool
      in
      if lower_bound then
        let lower_bound =
          get_docker conn builder variant ~lower_bound:true tag label pool
        in
        Current.list_seq [ upper_bound; lower_bound ]
      else Current.list_seq [ upper_bound ]

let get_local ~arch ~label ~builder ~pool ~distro ~ocaml_version ~host_base
    ~opam_version ~lower_bound base =
  match Variant.v ~arch ~distro ~ocaml_version ~opam_version with
  | Error (`Msg m) -> Current.fail m
  | Ok variant ->
      let upper_bound =
        get_docker_local builder variant ~lower_bound:false host_base base label
          pool
      in
      if lower_bound then
        let lower_bound =
          get_docker_local builder variant ~lower_bound:true host_base base
            label pool
        in
        Current.list_seq [ upper_bound; lower_bound ]
      else Current.list_seq [ upper_bound ]

let pull ~arch ~schedule ~builder ~distro ~ocaml_version ~opam_version =
  match Variant.v ~arch ~distro ~ocaml_version ~opam_version with
  | Error (`Msg m) -> Current.fail m
  | Ok variant ->
      let archl = Ocaml_version.to_opam_arch arch in
      let opam_version = Opam_version.to_string opam_version in
      Current.component "pull@,%s %a %s opam-%s" distro Ocaml_version.pp
        ocaml_version archl opam_version
      |> let> () = Current.return () in
         let tag = Variant.docker_tag variant in
         Builder.pull ~schedule ~arch builder @@ "ocaml/opam:" ^ tag

let peek ~arch ~schedule ~builder ~distro ~ocaml_version ~opam_version =
  match Variant.v ~arch ~distro ~ocaml_version ~opam_version with
  | Error (`Msg m) -> Current.fail m
  | Ok variant ->
      let archl = Ocaml_version.to_opam_arch arch in
      let opam_version = Opam_version.to_string opam_version in
      Current.component "peek@,%s %a %s opam-%s" distro Ocaml_version.pp
        ocaml_version archl opam_version
      |> let> () = Current.return () in
         let tag = Variant.docker_tag variant in
         Builder.peek ~schedule ~arch builder @@ "ocaml/opam:" ^ tag
