open Current.Syntax
open Ocaml_ci
open Pipeline
module Conf = Ocaml_ci_service.Conf
module Git = Current_git
module Gitlab = Current_gitlab
module Docker = Current_docker.Default

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "gitlab"

  let repositories_total =
    let help = "Total number of active repositories" in
    Gauge.v_label ~label_name:"account" ~help ~namespace ~subsystem
      "repositories_total"
end

let program_name = "ocaml-ci"

(* Link for GitLab statuses. *)
let url ~owner ~name ~hash =
  Uri.of_string
    (Printf.sprintf "%s/gitlab/%s/%s/commit/%s" Conf.website_scheme_and_domain
       owner name hash)

(* Watch the opam-repository for changes. *)
let opam_repository_commit =
  let module Github = Current_github in
  let repo = { Github.Repo_id.owner = "ocaml"; name = "opam-repository" } in
  Github.Api.Anonymous.head_of repo @@ `Ref "refs/heads/master"

(* TODO Sometime later, grab these from Index/DB table. *)
let gitlab_repos : Gitlab.Repo_id.t list =
  [
    {
      Gitlab.Repo_id.owner = "tmcgilchrist";
      Gitlab.Repo_id.name = "ocaml-gitlab";
      project_id = 29798678;
    };
    {
      Gitlab.Repo_id.owner = "talex5";
      Gitlab.Repo_id.name = "gemini-eio";
      project_id = 28169706;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "resto";
      project_id = 16269987;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "data-encoding";
      project_id = 14134943;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "ringo";
      project_id = 15200071;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "ocaml-secp256k1-internal";
      project_id = 17524462;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "lwt-exit";
      project_id = 22943026;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "lwt-watcher";
      project_id = 14672501;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "lwt-canceler";
      project_id = 14702762;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "ctypes_stubs_js";
      project_id = 31956063;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "seqes";
      project_id = 41118330;
    };
    {
      Gitlab.Repo_id.owner = "nomadic-labs";
      Gitlab.Repo_id.name = "tezt";
      project_id = 41852995;
    };
    {
      Gitlab.Repo_id.owner = "raphael-proust";
      Gitlab.Repo_id.name = "queenshead";
      project_id = 50742201;
    };
    {
      Gitlab.Repo_id.owner = "raphael-proust";
      Gitlab.Repo_id.name = "entombed";
      project_id = 30307357;
    };
  ]

(* Fake Installation module, we don't have this in GitLab. *)
module Installation = struct
  type t = { name : string }

  let compare = compare
  let pp f t = Fmt.string f t.name
end

let gitlab_installations =
  gitlab_repos
  |> List.map (fun x -> { Installation.name = x.Gitlab.Repo_id.owner })
  |> List.sort_uniq Installation.compare

let set_active_installations (accounts : Installation.t list Current.t) =
  let+ accounts in
  accounts
  |> List.fold_left
       (fun acc i -> Index.Owner_set.add i.Installation.name acc)
       Index.Owner_set.empty
  |> Index.set_active_owners;
  accounts

let set_active_repos ~installation repos =
  let+ repos and+ installation in
  repos
  |> List.fold_left
       (fun acc r -> Index.Repo_set.add r.Gitlab.Repo_id.name acc)
       Index.Repo_set.empty
  |> Index.set_active_repos ~owner:installation.Installation.name;
  Prometheus.Gauge.set
    (Metrics.repositories_total installation.Installation.name)
    (float_of_int (List.length repos));
  repos

let gref_from_commit x = Git.Commit_id.gref @@ Gitlab.Api.Commit.id x

let ref_name c =
  match (Gitlab.Api.Commit.branch_name c, Gitlab.Api.Commit.mr_name c) with
  | Some name, None | None, Some name -> name
  | _ -> failwith "Commit is neither a branch nor a MR"

let set_active_refs ~(repo : Gitlab.Repo_id.t Current.t) ~default commits =
  let+ repo and+ commits and+ default in
  let repo = { Repo_id.owner = repo.owner; name = repo.name } in
  let refs =
    commits
    |> List.fold_left
         (fun acc commit ->
           let commit_id = Gitlab.Api.Commit.id commit in
           let gref = Git.Commit_id.gref commit_id in
           let hash = Git.Commit_id.hash commit_id in
           let name = ref_name commit in
           let message = Gitlab.Api.Commit.message commit in
           Index.Ref_map.add gref { Index.hash; message; name } acc)
         Index.Ref_map.empty
  in
  let default_gref = gref_from_commit default in
  Index.set_active_refs ~repo refs default_gref;
  commits

let repositories (installation : Installation.t Current.t) =
  let+ installation in
  List.filter
    (fun repo -> repo.Gitlab.Repo_id.owner == installation.name)
    gitlab_repos

let gitlab_status_of_state head result =
  let+ head and+ result in
  let { Gitlab.Repo_id.owner; name; project_id = _ } =
    Gitlab.Api.Commit.repo_id head
  in
  let hash = Gitlab.Api.Commit.hash head in
  let url = url ~owner ~name ~hash in
  match result with
  | Ok () ->
      Gitlab.Api.Status.v ~url `Success ~description:"Passed" ~name:program_name
  | Error (`Active _) -> Gitlab.Api.Status.v ~url `Pending ~name:program_name
  | Error (`Msg m) when Astring.String.is_prefix ~affix:"[SKIP]" m ->
      Gitlab.Api.Status.v ~url `Success ~description:m ~name:program_name
  | Error (`Msg m) ->
      Gitlab.Api.Status.v ~url `Failure ~description:m ~name:program_name

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
     let result = summarise results in
     Current_incr.const (result, None)

let v ?ocluster ~app ~query_uri ~solver ~migrations () =
  let platforms =
    Conf.fetch_platforms ~query_uri ~include_macos:true ~include_freebsd:true
      ~include_windows:true ()
  in
  let ocluster =
    Option.map (Cluster_build.config ~timeout:(Duration.of_hour 1)) ocluster
  in
  let migrations =
    match migrations with
    | Some path -> Index.migrate path
    | None -> Current.return ()
  in
  Current.with_context migrations @@ fun () ->
  Current.with_context opam_repository_commit @@ fun () ->
  Current.with_context platforms @@ fun () ->
  Current.return gitlab_installations
  |> set_active_installations
  |> Current.list_iter ~collapse_key:"org" (module Installation)
     @@ fun installation ->
     repositories installation
     |> set_active_repos ~installation
     |> Current.list_iter ~collapse_key:"repo" (module Gitlab.Repo_id)
        @@ fun repo ->
        let* repo_id = repo in
        let default = Gitlab.Api.head_commit app repo_id in
        let refs =
          let refs =
            Gitlab.Api.ci_refs app ~staleness:Conf.max_staleness repo_id
          in
          set_active_refs ~repo ~default refs
        in
        refs
        |> Current.list_iter (module Gitlab.Api.Commit) @@ fun head ->
           let src = Git.fetch (Current.map Gitlab.Api.Commit.id head) in
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
                 let gref = Git.Commit_id.gref @@ Gitlab.Api.Commit.id commit in
                 let repo = Gitlab.Api.Commit.repo_id commit in
                 let repo = { Repo_id.owner = repo.owner; name = repo.name } in
                 let hash = Gitlab.Api.Commit.hash commit in
                 Some
                   (fun _ -> Index.record_summary_on_cancel ~repo ~gref ~hash)
           in
           let builds =
             let repo =
               Current.map
                 (fun repo ->
                   {
                     Repo_id.owner = repo.Gitlab.Repo_id.owner;
                     name = repo.name;
                   })
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
             let gref = Git.Commit_id.gref @@ Gitlab.Api.Commit.id commit in
             let repo =
               Gitlab.Api.Commit.repo_id commit |> fun repo ->
               { Repo_id.owner = repo.owner; name = repo.name }
             in
             let hash = Gitlab.Api.Commit.hash commit in
             let jobs =
               List.map
                 (fun (s, (_, job_id)) -> (s.Build_info.label, job_id))
                 builds
             in
             Index.record ~repo ~hash ~status ~gref jobs
           and set_gitlab_status =
             gitlab_status_of_state head summary
             |> Gitlab.Api.Commit.set_status head "ocaml-ci"
           in
           Current.all [ index; set_gitlab_status ]
