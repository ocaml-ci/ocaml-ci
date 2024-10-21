module Service = Ocaml_ci_service
module OV = Ocaml_version
module OVR = OV.Releases
module DD = Dockerfile_opam.Distro

(* type t = string * OV.t * OV.arch *)

let extract { Service.Conf.distro; ocaml_version; arch; _ } =
  (distro, OV.with_just_major_and_minor ocaml_version, arch)

let eq a b = a = b
let distro_arch_eq (d0, a0) (d1, _, a1) = d0 = d1 && a0 = a1

let pp (distro, ov, arch) =
  Printf.sprintf "%s-%s-%s" distro (OV.to_string ov) (OV.string_of_arch arch)

let pp_no_ov (distro, arch) =
  Printf.sprintf "%s-[any]-%s" distro (OV.string_of_arch arch)

let to_tag d = DD.((resolve_alias d :> t) |> tag_of_distro)

let test_platforms () =
  let platforms =
    Service.Conf.platforms ~profile:`All ~include_macos:true
      ~include_freebsd:true ~include_windows:true `V2_1
    |> List.map extract
  in
  let exists =
    [
      (true, (to_tag (`Debian `Stable), OVR.latest, `X86_64));
      (true, (to_tag (`Debian `Stable), OVR.latest, `Aarch64));
      (true, (to_tag (`Debian `Stable), OVR.latest, `S390x));
      (true, (to_tag (`Debian `Stable), OVR.latest, `Ppc64le));
      (true, (to_tag (`Debian `Stable), OVR.latest, `I386));
      (true, (to_tag (`Debian `Stable), OVR.latest, `Aarch32));
      (true, (to_tag (`Debian `Stable), OVR.v4_14, `X86_64));
      (true, (to_tag (`Debian `Stable), OVR.v4_14, `Aarch64));
      (true, (to_tag (`Debian `Stable), OVR.v4_14, `S390x));
      (true, (to_tag (`Debian `Stable), OVR.v4_14, `Ppc64le));
      (true, (to_tag (`Debian `Stable), OVR.v4_14, `I386));
      (true, (to_tag (`Debian `Stable), OVR.v4_14, `Aarch32));
      (true, (to_tag (`Fedora `Latest), OVR.latest, `X86_64));
      (true, (to_tag (`Fedora `Latest), OVR.v4_14, `X86_64));
      (true, (to_tag (`Alpine `Latest), OVR.latest, `X86_64));
      (true, (to_tag (`Alpine `Latest), OVR.v4_14, `X86_64));
      (true, (to_tag (`OpenSUSE `Latest), OVR.latest, `X86_64));
      (true, (to_tag (`OpenSUSE `Latest), OVR.v4_14, `X86_64));
      (true, (to_tag (`Ubuntu `Latest), OVR.latest, `X86_64));
      (true, (to_tag (`Ubuntu `Latest), OVR.v4_14, `X86_64));
    ]
    |> List.map (fun (a, (b, ov, c)) ->
           (a, (b, OV.with_just_major_and_minor ov, c)))
  in
  List.iter
    (fun (expect, p) ->
      Alcotest.(check bool) (pp p) expect (List.exists (eq p) platforms))
    exists

let test_macos_platforms () =
  let platforms =
    Service.Conf.platforms ~profile:`All ~include_macos:true
      ~include_freebsd:true ~include_windows:true `V2_1
    |> List.map extract
  in
  let exists =
    [
      (true, ("macos-homebrew", OVR.v5_2, `Aarch64));
      (true, ("macos-homebrew", OVR.v5_2, `X86_64));
      (true, ("macos-homebrew", OVR.v4_14, `Aarch64));
      (true, ("macos-homebrew", OVR.v4_14, `X86_64));
    ]
    |> List.map (fun (a, (b, ov, c)) ->
           (a, (b, OV.with_just_major_and_minor ov, c)))
  in
  List.iter
    (fun (expect, p) ->
      Alcotest.(check bool) (pp p) expect (List.exists (eq p) platforms))
    exists;
  (* Test that macos doesn't occur with these arches under any OCaml version *)
  let exists =
    [
      (false, ("macos-homebrew", `S390x));
      (false, ("macos-homebrew", `Ppc64le));
      (false, ("macos-homebrew", `I386));
      (false, ("macos-homebrew", `Riscv64));
      (false, ("macos-homebrew", `Aarch32));
    ]
  in
  List.iter
    (fun (expect, p) ->
      Alcotest.(check bool)
        (pp_no_ov p) expect
        (List.exists (distro_arch_eq p) platforms))
    exists

let test_distro_arches () =
  let platforms =
    Service.Conf.platforms ~profile:`All ~include_macos:true
      ~include_freebsd:true ~include_windows:true `V2_1
    |> List.map extract
  in
  (* Test that these distros don't occur with these arches under any OCaml version *)
  let exists =
    [
      (false, (to_tag (`CentOS `Latest), `X86_64));
      (false, (to_tag (`OracleLinux `Latest), `X86_64));
      (false, (to_tag (`Debian `Stable), `Riscv64));
    ]
  in
  List.iter
    (fun (expect, p) ->
      Alcotest.(check bool)
        (pp_no_ov p) expect
        (List.exists (distro_arch_eq p) platforms))
    exists

let tests =
  [
    Alcotest.test_case "test_platforms" `Quick test_platforms;
    Alcotest.test_case "test_macos_platforms" `Quick test_macos_platforms;
    Alcotest.test_case "test_distro_arches" `Quick test_distro_arches;
  ]
