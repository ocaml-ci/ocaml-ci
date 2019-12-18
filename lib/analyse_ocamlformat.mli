type source =
  | Opam of { version : string } (** Should install OCamlformat from Opam. *)
  | Vendored of { path : string } (** OCamlformat is vendored. [path] is relative from the project's root. *)
[@@deriving yojson,eq]

val get_ocamlformat_source : Current.Job.t -> opam_files:string list -> Fpath.t -> source option Lwt.t
(** Detect the required version of ocamlformat or if it's vendored.
    Vendored OCamlformat is detected by looking at file names in [opam_files]. *)
