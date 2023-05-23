type t_in = ([ `Built | `Checked ], [ `Active of unit | `Msg of string ]) result
[@@deriving show, eq]

let t_in = Alcotest.testable pp_t_in equal_t_in

type t_ret = (unit, [ `Active of [ `Running ] | `Msg of string ]) result
[@@deriving show, eq]

let t_ret = Alcotest.testable pp_t_ret equal_t_ret

let test_summarise_success () =
  let result =
    Ocaml_ci.Pipeline.
      [
        (build_info_of_label "build_1", (Result.Ok `Built, ()));
        (build_info_of_label "build_2", (Result.Ok `Built, ()));
        (build_info_of_label "lint_1", (Result.Ok `Checked, ()));
      ]
    |> Ocaml_ci.Pipeline.summarise
  in
  let expected = Result.Ok () in
  Alcotest.(check t_ret) "Success" expected result

let test_summarise_fail () =
  let result =
    Ocaml_ci.Pipeline.
      [
        (build_info_of_label "build_1", (Result.Ok `Built, ()));
        (build_info_of_label "build_2", (Result.Ok `Built, ()));
        (build_info_of_label "lint_1", (Result.Ok `Checked, ()));
        (build_info_of_label "build_3", (Result.Error (`Msg "msg"), ()));
      ]
    |> Ocaml_ci.Pipeline.summarise
  in
  let expected = Result.Error (`Msg "build_3 failed: msg") in
  Alcotest.(check t_ret) "Failed" expected result

let test_summarise_running () =
  let result =
    Ocaml_ci.Pipeline.
      [
        (build_info_of_label "build_1", (Result.Ok `Built, ()));
        (build_info_of_label "build_2", (Result.Ok `Built, ()));
        (build_info_of_label "lint_1", (Result.Ok `Checked, ()));
        (build_info_of_label "lint_2", (Result.Error (`Active ()), ()));
      ]
    |> Ocaml_ci.Pipeline.summarise
  in
  let expected = Result.Error (`Active `Running) in
  Alcotest.(check t_ret) "Running" expected result

let test_summarise_success_experimental_fail () =
  let result =
    Ocaml_ci.Pipeline.
      [
        (build_info_of_label "build_1", (Result.Ok `Built, ()));
        (build_info_of_label "build_2", (Result.Ok `Built, ()));
        (build_info_of_label "lint_1", (Result.Ok `Checked, ()));
        ( build_info_of_label "(lint-lower-bounds)",
          (Result.Error (`Msg "failed"), ()) );
      ]
    |> Ocaml_ci.Pipeline.summarise
  in
  let expected = Result.Ok () in
  Alcotest.(check t_ret) "Success" expected result

let test_summarise_success_experimental_running () =
  let result =
    Ocaml_ci.Pipeline.
      [
        (build_info_of_label "build_1", (Result.Ok `Built, ()));
        (build_info_of_label "build_2", (Result.Ok `Built, ()));
        (build_info_of_label "lint_1", (Result.Ok `Checked, ()));
        ( build_info_of_label "(lint-lower-bounds)",
          (Result.Error (`Active ()), ()) );
      ]
    |> Ocaml_ci.Pipeline.summarise
  in
  let expected = Result.Ok () in
  Alcotest.(check t_ret) "Success" expected result

let test_experimental () =
  let variant distro ocaml_version =
    Result.get_ok
    @@ Ocaml_ci.Variant.v ~arch:`Aarch64 ~distro ~ocaml_version
         ~opam_version:`V2_1
  in
  let v =
    Ocaml_ci.(
      Pipeline.
        [
          (true, build_info_of_label "(lint-lower-bounds)");
          (false, build_info_of_label "(lint-doc)");
          ( true,
            {
              label = "";
              variant =
                Some (variant "macos-homebrew" Ocaml_version.Releases.v5_0);
            } );
          ( true,
            {
              label = "";
              variant = Some (variant "distro" Ocaml_version.Releases.v5_1);
            } );
          ( true,
            {
              label = "";
              variant =
                Some
                  (variant "distro"
                     (Ocaml_version.v 5 1 ~patch:0 ~prerelease:"alpha1"));
            } );
          ( false,
            {
              label = "";
              variant = Some (variant "distro" Ocaml_version.Releases.v5_0);
            } );
        ])
  in
  let expected = List.map fst v in
  let result =
    List.map (fun v -> snd v |> Ocaml_ci.Pipeline.experimental_variant) v
  in
  List.iter2 (Alcotest.(check bool) "Success") expected result

let tests =
  [
    Alcotest.test_case "summarise_success" `Quick test_summarise_success;
    Alcotest.test_case "summarise_fail" `Quick test_summarise_fail;
    Alcotest.test_case "summarise_running" `Quick test_summarise_running;
    Alcotest.test_case "summarise_success_experimental_fail" `Quick
      test_summarise_success_experimental_fail;
    Alcotest.test_case "summarise_success_experimental_running" `Quick
      test_summarise_success_experimental_running;
    Alcotest.test_case "experimental" `Quick test_experimental;
  ]
