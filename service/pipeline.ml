open Current.Syntax
open Ocaml_ci
open Pipeline
module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash ~gref =
  Uri.of_string
    (Printf.sprintf "%s/github/%s/%s/commit/%s/-/%s"
       Conf.website_scheme_and_domain owner name hash gref)

(* Link for GitHub CheckRun details. *)
let url_variant ~owner ~name ~hash ~variant ~gref =
  Printf.sprintf "%s/github/%s/%s/commit/%s/variant/%s/-/%s"
    Conf.website_scheme_and_domain owner name hash variant gref

let opam_repository_commit =
  let repo = { Github.Repo_id.owner = "ocaml"; name = "opam-repository" } in
  Github.Api.Anonymous.head_of repo @@ `Ref "refs/heads/master"

let pp_fail prefix f m = Fmt.pf f "%s: %s" prefix (Ansi.strip m)

let github_status_of_state ~head result results =
  let+ head and+ result and+ results in
  let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
  let commit_id = Github.Api.Commit.id head in
  let gref = Current_git.Commit_id.gref commit_id in
  let hash = Github.Api.Commit.hash head in
  let url = url ~owner ~name ~hash ~gref in
  let pp_status f = function
    | s, (build, _job_id) -> (
        let label = s.Build_info.label in
        let job_url = url_variant ~owner ~name ~hash ~variant:label ~gref in
        match build with
        | Ok `Checked | Ok `Built ->
            Fmt.pf f "%s [%s (%s)](%s)" "✅" label "passed" job_url
        | Error (`Msg m) when Astring.String.is_prefix ~affix:"[SKIP]" m ->
            Fmt.pf f "%s [%s (%s)](%s)" "¯\\_(ツ)_/¯" label "skipped" job_url
        | Error (`Msg m) when Build_info.experimental_variant s ->
            Fmt.pf f "%s [EXPERIMENTAL: %s (%a)](%s)" "❌" label
              (pp_fail "failure") m job_url
        | Error (`Msg m) ->
            Fmt.pf f "%s [%s (%a)](%s)" "❌" label (pp_fail "failed") m job_url
        | Error (`Active _) ->
            Fmt.pf f "%s [%s (%s)](%s)" "🟠" label "active" job_url)
  in
  let summary =
    Fmt.str "@[<v>%a@]"
      (Fmt.list ~sep:Fmt.cut pp_status)
      (List.sort
         (fun (x, _) (y, _) ->
           String.compare x.Build_info.label y.Build_info.label)
         results)
  in
  match result with
  | Ok _ -> Github.Api.CheckRunStatus.v ~url (`Completed `Success) ~summary
  | Error (`Active _) -> Github.Api.CheckRunStatus.v ~url `Queued ~summary
  | Error (`Msg m) when Astring.String.is_prefix ~affix:"[SKIP]" m ->
      Github.Api.CheckRunStatus.v ~url (`Completed (`Skipped m)) ~summary
  | Error (`Msg m) ->
      Github.Api.CheckRunStatus.v ~url (`Completed (`Failure m)) ~summary

let set_active_installations installations =
  let+ installations in
  installations
  |> List.fold_left
       (fun acc i -> Index.Owner_set.add (Github.Installation.account i) acc)
       Index.Owner_set.empty
  |> Index.set_active_owners;
  installations

let set_active_repos ~installation repos =
  let+ installation and+ repos in
  let owner = Github.Installation.account installation in
  repos
  |> List.fold_left
       (fun acc r -> Index.Repo_set.add (Github.Api.Repo.id r).name acc)
       Index.Repo_set.empty
  |> Index.set_active_repos ~owner;
  repos

let ref_from_commit (x : Github.Api.Commit.t) : string =
  Git.Commit_id.gref @@ Github.Api.Commit.id x

let ref_name c =
  match (Github.Api.Commit.pr_name c, Github.Api.Commit.branch_name c) with
  | Some x, None -> x
  | None, Some x -> x
  | _ -> failwith "Commit is neither a branch nor a PR"

let set_active_refs ~repo refs default_ref =
  let+ repo and+ xs = refs and+ default = default_ref in
  let github_repo = Github.Api.Repo.id repo in
  let repo = { Repo_id.owner = github_repo.owner; name = github_repo.name } in
  let refs =
    xs
    |> List.fold_left
         (fun acc x ->
           let gref = ref_from_commit x in
           let commit = Github.Api.Commit.id x in
           let hash = Git.Commit_id.hash commit in
           let name = ref_name x in
           let message = Github.Api.Commit.message x in
           Index.Ref_map.add gref { Index.hash; message; name } acc)
         Index.Ref_map.empty
  in
  Index.set_active_refs ~repo refs (ref_from_commit default);
  xs

let local_test ~query_uri ~solver repo () =
  let platforms =
    Conf.fetch_platforms ~query_uri ~include_macos:false ~include_freebsd:false
      ~include_windows:false ()
  in
  let src = Git.Local.head_commit repo in
  let src_content = Repo_content.extract src in
  let repo = Current.return { Repo_id.owner = "local"; name = "test" }
  and analysis =
    Analyse.examine ~solver ~platforms ~opam_repository_commit src src_content
  in
  Current.component "summarise"
  |> let> results = build_with_docker ~repo ~analysis ~platforms src in
     let result = results |> summarise in
     Current_incr.const (result, None)

let v ?ocluster ~app ~solver ~query_uri ~migrations () =
  let ocluster =
    Option.map (Cluster_build.config ~timeout:(Duration.of_hour 1)) ocluster
  in
  let platforms =
    Conf.fetch_platforms ~query_uri ~include_macos:true ~include_freebsd:true
      ~include_windows:true ()
  in
  let migrations =
    match migrations with
    | Some path -> Index.migrate path
    | None -> Current.return ()
  in
  Current.with_context migrations @@ fun () ->
  Current.with_context opam_repository_commit @@ fun () ->
  Current.with_context platforms @@ fun () ->
  Github.App.installations app
  |> set_active_installations
  |> Current.list_iter ~collapse_key:"org" (module Github.Installation)
     @@ fun installation ->
     Github.Installation.repositories installation
     |> set_active_repos ~installation
     |> Current.list_iter ~collapse_key:"repo" (module Github.Api.Repo)
        @@ fun repo ->
        let default = Github.Api.Repo.head_commit repo in
        let refs =
          let refs =
            Github.Api.Repo.ci_refs ~staleness:Conf.max_staleness repo
          in
          set_active_refs ~repo refs default
        in
        refs
        |> Current.list_iter ~collapse_key:"ref" (module Github.Api.Commit)
           @@ fun head ->
           let src = Git.fetch (Current.map Github.Api.Commit.id head) in
           let src_content = Repo_content.extract src in
           let analysis =
             Analyse.examine ~solver ~platforms ~opam_repository_commit src
               src_content
           in
           let* on_cancel =
             match ocluster with
             | None -> Current.return None
             | Some _ ->
                 let+ commit = head in
                 let gref = ref_from_commit commit in
                 let repo = Current_github.Api.Commit.repo_id commit in
                 let repo = { Repo_id.owner = repo.owner; name = repo.name } in
                 let hash = Current_github.Api.Commit.hash commit in
                 Some
                   (fun _ -> Index.record_summary_on_cancel ~repo ~gref ~hash)
           in
           let builds =
             let repo =
               Current.map
                 (fun x ->
                   Github.Api.Repo.id x |> fun repo ->
                   { Repo_id.owner = repo.owner; name = repo.name })
                 repo
             in
             build_with_docker ?ocluster ?on_cancel ~repo ~analysis ~platforms
               src
           in
           let summary = Current.map summarise builds in
           let status =
             let+ summary in
             match summary with
             | Ok () -> `Passed
             | Error (`Active `Running) -> `Pending
             | Error (`Msg _) -> `Failed
           in
           let index =
             let+ commit = head and+ builds and+ status in
             let gref = ref_from_commit commit in
             let repo =
               Current_github.Api.Commit.repo_id commit |> fun repo ->
               { Repo_id.owner = repo.owner; name = repo.name }
             in
             let hash = Current_github.Api.Commit.hash commit in
             let jobs =
               List.map
                 (fun (s, (_, job_id)) -> (s.Build_info.label, job_id))
                 builds
             in
             Index.record ~repo ~hash ~status ~gref jobs
           and set_github_status =
             builds
             |> github_status_of_state ~head summary
             |> Github.Api.CheckRun.set_status head "ocaml-ci"
           in
           Current.all [ index; set_github_status ]
