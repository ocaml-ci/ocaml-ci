type t = { label : string; variant : Variant.t option }
(** [variant] will be [None] for utility or linting jobs without a platform, and
    [Some v] for jobs building on a particular platform [v]. *)

let of_spec = function
  | Spec.{ label; variant; ty = _ } -> { label; variant = Some variant }

let of_label label = { label; variant = None }

(** Check whether a variant is considered experimental.

    If it is experimental we allow those builds to fail without failing the
    overall build for a commit. *)
let experimental_variant s =
  if
    Astring.String.(
      is_prefix ~affix:Variant.lower_bound_label s.label
      || is_prefix ~affix:Variant.opam_label s.label)
  then true
  else
    match s.variant with
    | None -> false
    | Some v -> Astring.String.equal "windows-server-2022" (Variant.distro v)

(** Like [experimental_variant], but takes strings for when a [build_info]
    record is unavailable.

    Prefer to use [experimental_variant] when possible, as this function can
    potentially give false positives. *)
let experimental_variant_str s =
  Astring.String.(
    is_prefix ~affix:Variant.lower_bound_label s
    || is_prefix ~affix:Variant.opam_label s
    || is_prefix ~affix:"windows-server-2022" s)
