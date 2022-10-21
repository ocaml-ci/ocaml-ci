let src = Logs.Src.create "ocaml_ci.index" ~doc:"ocaml-ci indexer"

module Log = (val Logs.src_log src : Logs.LOG)
module Db = Current.Db
module Job_map = Astring.String.Map

type t = {
  record_job : Sqlite3.stmt;
  remove : Sqlite3.stmt;
  get_jobs : Sqlite3.stmt;
  get_job : Sqlite3.stmt;
  get_job_ids : Sqlite3.stmt;
  (* get_commits_job_ids_for_ref : Sqlite3.stmt; *)
  get_git_fetch_outcome_by_time : Sqlite3.stmt;
  full_hash : Sqlite3.stmt;
}

type job_state =
  [ `Not_started | `Active | `Failed of string | `Passed | `Aborted ]
[@@deriving show]

type build_status = [ `Not_started | `Pending | `Failed | `Passed ]

type ref_info = {
  hash : string;
  message : string;
  gref : string;
  started_at : float option;
}

let or_fail label x =
  match x with
  | Sqlite3.Rc.OK -> ()
  | err -> Fmt.failwith "Sqlite3 %s error: %s" label (Sqlite3.Rc.to_string err)

let is_valid_hash hash =
  let open Astring in
  String.length hash >= 6 && String.for_all Char.Ascii.is_alphanum hash

let db =
  lazy
    (let db = Lazy.force Current.Db.v in
     Current_cache.Db.init ();
     let gref_col_does_not_exist =
       let q =
         Sqlite3.prepare db
           "SELECT count(*) FROM pragma_table_info('ci_build_index') WHERE \
            name='gref'"
       in
       Db.query_one q [] = Sqlite3.Data.[ INT 0L ]
     in
     Sqlite3.exec db
       {|
CREATE TABLE IF NOT EXISTS ci_build_index (
  owner     TEXT NOT NULL,
  name      TEXT NOT NULL,
  hash      TEXT NOT NULL,
  variant   TEXT NOT NULL,
  job_id    TEXT,
  PRIMARY KEY (owner, name, hash, variant)
)|}
     |> or_fail "create table";
     if gref_col_does_not_exist then
       let () = Log.info (fun f -> f "ADDING gref col to ci_build_index.") in
       Sqlite3.exec db
         {|
ALTER TABLE ci_build_index
  ADD COLUMN gref TEXT NOT NULL DEFAULT "-"
    |}
       |> or_fail "alter table"
     else Log.info (fun f -> f "gref col detected.");
     let record_job =
       Sqlite3.prepare db
         "INSERT OR REPLACE INTO ci_build_index (owner, name, hash, variant, \
          gref, job_id) VALUES (?, ?, ?, ?, ?, ?)"
     and remove =
       Sqlite3.prepare db
         "DELETE FROM ci_build_index WHERE owner = ? AND name = ? AND hash = ? \
          AND variant = ?"
     and get_jobs =
       Sqlite3.prepare db
         "SELECT ci_build_index.variant, ci_build_index.job_id, cache.ok, \
          cache.outcome FROM ci_build_index LEFT JOIN cache ON \
          ci_build_index.job_id = cache.job_id WHERE ci_build_index.owner = ? \
          AND ci_build_index.name = ? AND ci_build_index.hash = ?"
     and get_job =
       Sqlite3.prepare db
         "SELECT job_id FROM ci_build_index WHERE owner = ? AND name = ? AND \
          hash = ? AND variant = ?"
     and get_job_ids =
       Sqlite3.prepare db
         "SELECT variant, job_id FROM ci_build_index WHERE owner = ? AND name \
          = ? AND hash = ?"
     (* and get_commits_job_ids_for_ref =
        Sqlite3.prepare db
          "SELECT variant, hash, job_id FROM ci_build_index WHERE owner = ? AND \
           name = ? AND gref = ?" *)
     and get_git_fetch_outcome_by_time =
       Sqlite3.prepare db
         " SELECT key, strftime('%s', ready) FROM cache WHERE op LIKE \
          'git-fetch' AND key LIKE ? ORDER BY ready DESC"
     and full_hash =
       Sqlite3.prepare db
         "SELECT DISTINCT hash FROM ci_build_index WHERE owner = ? AND name = \
          ? AND hash LIKE ?"
     in
     {
       record_job;
       remove;
       get_jobs;
       get_job;
       get_job_ids;
       (* get_commits_job_ids_for_ref; *)
       get_git_fetch_outcome_by_time;
       full_hash;
     })

let init () = ignore (Lazy.force db)

let get_job_ids_with_variant t ~owner ~name ~hash =
  Db.query t.get_job_ids Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash ]
  |> List.map @@ function
     | Sqlite3.Data.[ TEXT variant; NULL ] -> (variant, None)
     | Sqlite3.Data.[ TEXT variant; TEXT id ] -> (variant, Some id)
     | row -> Fmt.failwith "get_job_ids: invalid row %a" Db.dump_row row

let get_build_history ~owner ~name ~gref =
  let t = Lazy.force db in
  let repo = Fmt.str "%%%s/%s.git %%" owner name in
  Logs.debug (fun l -> l "Trying to fetch %s on %s" repo gref);
  Db.query t.get_git_fetch_outcome_by_time Sqlite3.Data.[ TEXT repo ]
  |> (List.map @@ function
      | Sqlite3.Data.[ BLOB key; TEXT ready ] -> (
          let key = String.split_on_char ' ' key in
          match key with
          | [ _; gref; hash ] ->
              (gref, hash, "Commit Title (history)", Float.of_string_opt ready)
          | _ -> Fmt.failwith "get_build_history: wrong key format")
      | row -> Fmt.failwith "get_build_history: invalid row %a" Db.dump_row row)
  |> List.filter (fun (gref', _, _, time) ->
         if Option.is_none time then false else gref = gref')
  |> List.map (fun (gref, hash, message, started_at) ->
         { gref; hash; message; started_at })

module Status_cache = struct
  let cache = Hashtbl.create 1_000
  let cache_max_size = 1_000_000

  type elt = [ `Not_started | `Pending | `Failed | `Passed ]

  let add ~owner ~name ~hash (status : elt) =
    if Hashtbl.length cache > cache_max_size then Hashtbl.clear cache;
    Hashtbl.add cache (owner, name, hash) status

  let find ~owner ~name ~hash : elt =
    Hashtbl.find_opt cache (owner, name, hash) |> function
    | Some s -> s
    | None -> `Not_started
end

let get_status = Status_cache.find

let record ~repo ~hash ~status ~message ~gref jobs =
  ignore message;
  let { Repo_id.owner; name } = repo in
  let t = Lazy.force db in
  let () = Status_cache.add ~owner ~name ~hash status in
  let jobs = Job_map.of_list jobs in
  let previous =
    get_job_ids_with_variant t ~owner ~name ~hash |> Job_map.of_list
  in
  let merge variant prev job =
    let set job_id =
      Log.info (fun f ->
          f "@[<h>Index.record %s/%s %s %s -> %a@]" owner name
            (Astring.String.with_range ~len:6 hash)
            variant
            Fmt.(option ~none:(any "-") string)
            job_id);
      match job_id with
      | None ->
          Db.exec t.record_job
            Sqlite3.Data.
              [
                TEXT owner; TEXT name; TEXT hash; TEXT variant; TEXT gref; NULL;
              ]
      | Some id ->
          Db.exec t.record_job
            Sqlite3.Data.
              [
                TEXT owner;
                TEXT name;
                TEXT hash;
                TEXT variant;
                TEXT gref;
                TEXT id;
              ]
    in
    let update j1 j2 =
      match (j1, j2) with
      | Some j1, Some j2 when j1 = j2 -> ()
      | None, None -> ()
      | _, j2 -> set j2
    in
    let remove () =
      Log.info (fun f ->
          f "@[<h>Index.record %s/%s %s %s REMOVED@]" owner name
            (Astring.String.with_range ~len:6 hash)
            variant);
      Db.exec t.remove
        Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash; TEXT variant ]
    in
    (match (prev, job) with
    | Some j1, Some j2 -> update j1 j2
    | None, Some j2 -> set j2
    | Some _, None -> remove ()
    | None, None -> assert false);
    None
  in
  let (_ : [ `Empty ] Job_map.t) = Job_map.merge merge previous jobs in
  ()

let get_full_hash ~owner ~name short_hash =
  let t = Lazy.force db in
  if is_valid_hash short_hash then
    match
      Db.query t.full_hash
        Sqlite3.Data.[ TEXT owner; TEXT name; TEXT (short_hash ^ "%") ]
    with
    | [] -> Error `Unknown
    | [ Sqlite3.Data.[ TEXT hash ] ] -> Ok hash
    | [ _ ] -> failwith "full_hash: invalid result!"
    | _ :: _ :: _ -> Error `Ambiguous
  else Error `Invalid

let get_jobs ~owner ~name hash =
  let t = Lazy.force db in
  Db.query t.get_jobs Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash ]
  |> List.map @@ function
     | Sqlite3.Data.[ TEXT variant; TEXT job_id; NULL; NULL ] ->
         let outcome =
           if Current.Job.lookup_running job_id = None then `Aborted
           else `Active
         in
         let ts = Run_time.timestamps_of_job job_id in
         (variant, outcome, ts)
     | Sqlite3.Data.[ TEXT variant; TEXT job_id; INT ok; BLOB outcome ] ->
         let outcome = if ok = 1L then `Passed else `Failed outcome in
         let ts = Run_time.timestamps_of_job job_id in
         (variant, outcome, ts)
     | Sqlite3.Data.[ TEXT variant; NULL; NULL; NULL ] ->
         (variant, `Not_started, None)
     | row -> Fmt.failwith "get_jobs: invalid result: %a" Db.dump_row row

let get_job ~owner ~name ~hash ~variant =
  let t = Lazy.force db in
  match
    Db.query_some t.get_job
      Sqlite3.Data.[ TEXT owner; TEXT name; TEXT hash; TEXT variant ]
  with
  | None -> Error `No_such_variant
  | Some Sqlite3.Data.[ TEXT id ] -> Ok (Some id)
  | Some Sqlite3.Data.[ NULL ] -> Ok None
  | _ -> failwith "get_job: invalid result!"

let get_job_ids ~owner ~name ~hash =
  let t = Lazy.force db in
  get_job_ids_with_variant t ~owner ~name ~hash |> List.filter_map snd

let get_message ~owner ~name ~hash =
  (* FIXME [benmandrew]: implement this *)
  ignore owner;
  ignore name;
  ignore hash;
  "Placeholder commit message"

module Owner_set = Set.Make (String)

let active_owners = ref Owner_set.empty
let set_active_owners x = active_owners := x
let get_active_owners () = !active_owners

module Description_map = Map.Make (String)
module N_repos_map = Map.Make (String)

let description = ref Description_map.empty
let n_repos = ref N_repos_map.empty

let set_description ~owner x =
  description := Description_map.add owner x !description

let get_description ~owner =
  (* FIXME [benmandrew]: there is probably a better default to have here *)
  Description_map.find_opt owner !description
  |> Option.value ~default:"Placeholder description"

let set_n_repos ~owner x = n_repos := N_repos_map.add owner x !n_repos

let get_n_repos ~owner =
  N_repos_map.find_opt owner !n_repos |> Option.value ~default:0

module Owner_map = Map.Make (String)
module Repo_set = Set.Make (String)

let active_repos = ref Owner_map.empty

let set_active_repos ~owner x =
  active_repos := Owner_map.add owner x !active_repos

let get_active_repos ~owner =
  Owner_map.find_opt owner !active_repos |> Option.value ~default:Repo_set.empty

module Repo_map = Map.Make (Repo_id)
module Ref_map = Map.Make (String)

let active_refs : string Ref_map.t Repo_map.t ref = ref Repo_map.empty

let set_active_refs ~repo refs =
  active_refs := Repo_map.add repo refs !active_refs

let get_active_refs repo =
  Repo_map.find_opt repo !active_refs |> Option.value ~default:Ref_map.empty

let is_main_ref r =
  match Astring.String.cuts ~sep:"/" r with
  | "refs" :: "heads" :: branch :: _ ->
      String.equal branch "main" || String.equal branch "master"
  | _ -> false

let get_main_ref repo =
  get_active_refs repo
  |> Ref_map.bindings
  |> List.filter (fun (ref, _) -> is_main_ref ref)
  |> function
  | [] -> None
  | r :: _ -> Some r
