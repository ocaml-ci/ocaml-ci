(** Specifications for different kinds of builds. *)

type ty =
  [ `Opam of [ `Build | `Lint of [ `Doc | `Opam ] ] * Selection.t * string list
  | `Opam_fmt of Selection.t * Analyse_ocamlformat.source option
  | `Opam_monorepo of Opam_monorepo.config ]
[@@deriving to_yojson, ord]

type t = { label : string; variant : Variant.t; ty : ty }

val opam :
  label:string ->
  selection:Selection.t ->
  analysis:Analyse.Analysis.t ->
  [ `Build | `Lint of [ `Doc | `Fmt | `Opam ] ] ->
  t

val lint_specs : analysis:Analyse.Analysis.t -> Selection.t list -> t list
val opam_monorepo : Opam_monorepo.config list -> t list
val pp : t Fmt.t
val compare : t -> t -> int
val label : t -> string
val pp_summary : ty Fmt.t
