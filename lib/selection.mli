type t = {
  variant : Variant.t;  (** The variant image to build on. *)
  packages : string list;  (** The selected packages ("name.version"). *)
  only_packages : string list; [@default []]
      (** Local root packages to include (empty to include all). *)
  commit : string;  (** A commit in opam-repository to use. *)
}
[@@deriving yojson, ord]
(** A set of packages for a single build. *)

val of_worker : root_pkgs:string list -> Ocaml_ci_api.Worker.Selection.t -> t
val remove_package : t -> package:string -> t
val filter_duplicate_opam_versions : t list -> t list
