(** Execute a build on the cluster. *)

type t

val config :
  ?timeout:int64 ->
  [ `Submission_f4e8a768b32a7c42 ] Capnp_rpc_lwt.Sturdy_ref.t ->
  t

val v :
  t ->
  ?on_cancel:(string -> unit) ->
  platforms:Platform.t list Current.t ->
  repo:Repo_id.t Current.t ->
  spec:Spec.t Current.t ->
  Current_git.Commit_id.t Current.t ->
  ([> `Built | `Checked ] Current_term.Output.t * Current.job_id option)
  Current.t
(** Build and test all the opam packages in a given build context on the given
    platform. [~repo] is the ID of the repository-under-test on a Git Forge
    (e.g. GitHub or GitLab).

    @param on_cancel The callback function to call if the job is cancelled.
    @param repo The ID of the repository-under-test on GitHub. *)
