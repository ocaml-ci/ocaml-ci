(** A platform on which we wish to perform test builds. *)

module Pool_name : sig
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

  val to_string : t -> string
  val of_string : string -> (t, [ `Msg of string ]) result
end

type t = {
  label : string;
  builder : Builder.t;
  pool : Pool_name.t; (* OCluster pool *)
  variant : Variant.t; (* e.g. "debian-10-ocaml-4.08" *)
  base : Current_docker.Raw.Image.t; (* Base image to use *)
  vars : Ocaml_ci_api.Worker.Vars.t;
}

val pp : t Fmt.t
val compare : t -> t -> int

val compiler_matches_major_and_minor :
  Ocaml_ci_api.Worker.Vars.t -> version:Ocaml_version.t -> bool
(** [compiler_matches_major_and_minor vars ~version] is [true] iff the compiler
    version in [vars] matches [version], considering only the major and minor
    parts of the version number. *)

val set_compiler_version :
  Ocaml_ci_api.Worker.Vars.t ->
  version:Ocaml_version.t ->
  Ocaml_ci_api.Worker.Vars.t

val get :
  arch:Ocaml_version.arch ->
  label:string ->
  conn:Current_ocluster.Connection.t ->
  builder:Builder.t ->
  pool:Pool_name.t ->
  distro:string ->
  ocaml_version:Ocaml_version.t ->
  opam_version:Opam_version.t ->
  lower_bound:bool ->
  string Current.t ->
  t list Current.t
(** [get ~label ~builder ~variant ~host_base base] creates a [t] by getting the
    opam variables from [host_base] and returning [base] for subsequent builds. *)

val get_local :
  arch:Ocaml_version.arch ->
  label:string ->
  builder:Builder.t ->
  pool:Pool_name.t ->
  distro:string ->
  ocaml_version:Ocaml_version.t ->
  host_base:Current_docker.Raw.Image.t Current.t ->
  opam_version:Opam_version.t ->
  lower_bound:bool ->
  Current_docker.Raw.Image.t Current.t ->
  t list Current.t
(** [get ~label ~builder ~variant ~host_base base] creates a [t] by getting the
    opam variables from [host_base] and returning [base] for subsequent builds. *)

val pull :
  arch:Ocaml_version.arch ->
  schedule:Current_cache.Schedule.t ->
  builder:Builder.t ->
  distro:string ->
  ocaml_version:Ocaml_version.t ->
  opam_version:Opam_version.t ->
  Current_docker.Raw.Image.t Current.t
(** [pull ~schedule ~builder ~distro ~ocaml_version] pulls
    "ocaml/opam:\{distro\}-ocaml-\{version\}" on [schedule]. *)

val peek :
  arch:Ocaml_version.arch ->
  schedule:Current_cache.Schedule.t ->
  builder:Builder.t ->
  distro:string ->
  ocaml_version:Ocaml_version.t ->
  opam_version:Opam_version.t ->
  string Current.t
(** [peek ~schedule ~builder ~distro ~ocaml_version] pulls
    "ocaml/opam:\{distro\}-ocaml-\{version\}" on [schedule]. *)
