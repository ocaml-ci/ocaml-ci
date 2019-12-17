open Current.Syntax
module Docker = Conf.Builder_amd1

let ocamlformat ~ocamlformat_source ~base ~src =
  let dockerfile =
    let open Dockerfile in
    let+ base = base
    and+ install_ocamlformat =
      let+ ocamlformat_source = ocamlformat_source in
      match ocamlformat_source with
      | Ocaml_ci.Analyse_ocamlformat.Vendored { path } ->
        run "opam pin add -yn ocamlformat %S" path
        @@ run "opam depext ocamlformat"
        @@ run "opam install --deps-only -y ocamlformat"
      | Opam { version } ->
        run "opam depext ocamlformat=%s" version
        @@ run "opam install ocamlformat=%s" version
    in
    from (Docker.Image.hash base)
    @@ run "opam install dune" (* Not the dune version the project use *)
    @@ workdir "src"
    @@ install_ocamlformat
    @@ copy ~chown:"opam" ~src:["./"] ~dst:"./" ()
  in
  let img =
    Docker.build ~label:"OCamlformat" ~pool:Docker.pool ~pull:false ~dockerfile (`Git src)
  in
  Docker.run ~label:"lint" img ~args:[ "sh"; "-c"; "dune build @fmt || (echo \"dune build @fmt failed\"; exit 2)" ]
