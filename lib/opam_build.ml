(* If the package's directory name doesn't contain a dot then opam will default to
   using the last known version, which is usually wrong. In particular, if a multi-project
   repository adds a new package with a constraint "{ =version }" on an existing one,
   this will fail because opam will pin the new package as "dev" but the old one with
   the version of its last release. *)
let maybe_add_dev ~dir name =
  if Fpath.is_current_dir dir || not (String.contains (Fpath.basename dir) '.')
  then name ^ ".dev"
  else name

(* Group opam files by directory.
   e.g. ["a/a1.opam"; "a/a2.opam"; "b/b1.opam"] ->
        [("a", ["a/a1.opam"; "a/a2.opam"], ["a1.dev"; "a2.dev"]);
         ("b", ["b/b1.opam"], ["b1.dev"])
        ] *)
let group_opam_files =
  ListLabels.fold_left ~init:[] ~f:(fun acc x ->
      let item = Fpath.v x in
      let dir = Fpath.parent item in
      let pkg =
        Filename.basename x |> Filename.chop_extension |> maybe_add_dev ~dir
      in
      match acc with
      | (prev_dir, prev_items, pkgs) :: rest when Fpath.equal dir prev_dir ->
          (prev_dir, x :: prev_items, pkg :: pkgs) :: rest
      | _ -> (dir, [ x ], [ pkg ]) :: acc)

let mkdir dirs =
  let dirs =
    dirs
    |> List.filter_map (fun (dir, _, _) ->
           if Fpath.is_current_dir dir then None
           else Some (Filename.quote (Fpath.to_string dir)))
  in
  match dirs with
  | [] -> []
  | dirs -> [ Obuilder_spec.run "mkdir -p %s" (String.concat " " dirs) ]

(* Generate instructions to copy all the files in [items] into the
   image, creating the necessary directories first, and then pin them all. *)
let pin_opam_files ~network ?work_dir groups =
  if groups = [] then []
  else
    let open Obuilder_spec in
    mkdir groups
    @ (groups
      |> List.map (fun (dir, files, _) ->
             copy files
               ~dst:
                 (let dir =
                    Option.map
                      (fun work_dir -> Fpath.( // ) work_dir dir)
                      work_dir
                    |> Option.value ~default:dir
                  in
                  Fpath.to_string dir)))
    @ [
        groups
        |> List.concat_map (fun (dir, _, pkgs) ->
               pkgs
               |> List.map (fun pkg ->
                      let dir =
                        Option.map
                          (fun work_dir -> Fpath.( // ) work_dir dir)
                          work_dir
                        |> Option.value ~default:dir
                      in
                      Printf.sprintf "opam pin add -yn %s %s" pkg
                        (Filename.quote (Fpath.to_string dir))))
        |> String.concat " && \n"
        |> run ~network "%s";
      ]

(* Get the packages directly in "." *)
let rec get_root_opam_packages = function
  | [] -> []
  | (dir, _, pkgs) :: _ when Fpath.is_current_dir dir -> pkgs
  | _ :: rest -> get_root_opam_packages rest

let download_cache = "opam-archives"

let install_project_deps ~opam_version ~opam_files ~selection =
  let { Selection.packages; commit; variant; only_packages; lower_bound = _ } =
    selection
  in
  let prefix =
    match Variant.os variant with
    | `macOS -> "~/local"
    | `windows
    | `linux -> "/usr"
    | `freeBSD -> "/usr/local"
  in
  let ln =
    match Variant.os variant with
    | `windows
    | `macOS -> "ln"
    | `linux | `freeBSD -> "sudo ln"
  in
  let groups = group_opam_files opam_files in
  let root_pkgs = get_root_opam_packages groups in
  let non_root_pkgs =
    packages
    |> List.filter (fun pkg ->
           (not (List.mem pkg root_pkgs))
           && not
                (Astring.String.take ~rev:true ~sat:(( <> ) '.') pkg
                |> String.equal "opam"))
  in
  let compatible_root_pkgs =
    if only_packages = [] then root_pkgs else only_packages
  in
  let open Obuilder_spec in
  let cache =
    match Variant.os variant with
    | `freeBSD | `linux ->
        [
          Obuilder_spec.Cache.v download_cache
            ~target:"/home/opam/.opam/download-cache";
        ]
    | `windows ->
        [
          Obuilder_spec.Cache.v download_cache
            ~target:"/Users/opam/AppData/local/opam/download-cache";
        ]
    | `macOS ->
        [
          Obuilder_spec.Cache.v download_cache
            ~target:"/Users/mac1000/.opam/download-cache";
          Obuilder_spec.Cache.v "homebrew"
            ~target:"/Users/mac1000/Library/Caches/Homebrew";
        ]
  in
  let network = [ "host" ] in
  let distro_extras =
    if Astring.String.is_prefix ~affix:"fedora" (Variant.id variant) then
      [ run ~network "sudo dnf install -y findutils" ] (* (we need xargs) *)
    else []
  in
  let network = [ "host" ] in

  let home_dir =
    match Variant.os selection.Selection.variant with
    | `windows
    | `macOS -> None
    | `linux -> Some "/src"
    | `freeBSD -> Some "/src"
  in
  let work_dir =
    match Variant.os selection.Selection.variant with
    | `windows
    | `macOS -> Some (Fpath.v "./src/")
    | `linux -> None
    | `freeBSD -> None
  in
  (* XXX: don't overwrite default config? *)
  let opamrc = "" in
  let opam_version_str = Opam_version.to_string opam_version in
  let compatible_root_pkgs = String.concat " " compatible_root_pkgs in
  let non_root_pkgs = String.concat " " non_root_pkgs in
  let opam_depext =
    match opam_version with
    | `V2_0 ->
        run ~network ~cache "opam depext --update -y %s $DEPS"
          compatible_root_pkgs
    | `V2_1 ->
        run ~network ~cache
          "opam update --depexts && opam install --cli=2.1 --depext-only -y %s \
           $DEPS"
          compatible_root_pkgs
    | `V2_2 ->
        run ~network ~cache
          "opam update --depexts && opam install --cli=2.2 --depext-only -y %s \
           $DEPS"
          compatible_root_pkgs
  in
  (if Variant.arch variant |> Ocaml_version.arch_is_32bit then
     [ shell [ "/usr/bin/linux32"; "/bin/sh"; "-c" ] ]
   else [])
  @ [ env "CLICOLOR_FORCE" "1" ]
  @ [ env "OPAMCOLOR" "always" ]
  @ (match home_dir with
    | Some home_dir -> [ Obuilder_spec.workdir home_dir ]
    | None -> [])
  @ distro_extras
  @ [
      run "%s -f %s/bin/opam-%s %s/bin/opam" ln prefix opam_version_str prefix;
      run "opam init --reinit%s -ni" opamrc;
      run "uname -rs && opam exec -- ocaml -version && opam --version";
    ]
  @ (match home_dir with
    | Some home_dir -> [ workdir home_dir; run "sudo chown opam /src" ]
    | None -> [])
  @ [
      run ~network ~cache
        "cd ~/opam-repository && (git cat-file -e %s || git fetch origin \
         master) && git reset -q --hard %s && git log --no-decorate -n1 \
         --oneline && opam update -u"
        commit commit;
    ]
  @ pin_opam_files ~network ?work_dir groups
  @ [
      env "DEPS" non_root_pkgs;
      env "CI" "true";
      env "OCAMLCI" "true";
      opam_depext;
      run ~network ~cache "opam install $DEPS";
    ]

let spec ~base ~opam_version ~opam_files ~selection =
  let open Obuilder_spec in
  let to_name x = OpamPackage.of_string x |> OpamPackage.name_to_string in
  let home_dir =
    match Variant.os selection.Selection.variant with
    | `windows
    | `macOS -> "./src"
    | `linux -> "/src"
    | `freeBSD -> "/src"
  in
  let only_packages =
    match selection.Selection.only_packages with
    | [] -> ""
    | pkgs -> " --only-packages=" ^ String.concat "," (List.map to_name pkgs)
  in
  let run_build =
    match Variant.os selection.Selection.variant with
    | `windows
    | `macOS ->
        run
          "cd ./src && opam exec -- dune build%s @install @check @runtest && \
           rm -rf _build"
          only_packages
    | `linux | `freeBSD ->
        run
          "opam exec -- dune build%s @install @check @runtest && rm -rf _build"
          only_packages
  in
  stage ~from:base
    (comment "%s" (Fmt.str "%a" Variant.pp selection.Selection.variant)
     :: user_unix ~uid:1000 ~gid:1000
     :: install_project_deps ~opam_version ~opam_files ~selection
    @ [ copy [ "." ] ~dst:home_dir; run_build ])
