open Cmdliner

(* Inspired by https://gitlab.ocamlpro.com/louis/opam-custom-install *)

let check_npm_deps_doc = "Check for npm depexts inside the node_modules folder"

let match_version_against_constraint version formula =
  let open EsyPackageConfig.SemverVersion in
  let pf = Formula.parseExn formula in
  let pv = Version.parseExn version in
  Formula.DNF.matches ~version:pv pf

let depexts nv opams =
  try
    let opam = OpamPackage.Map.find nv opams in
    List.fold_left
      (fun npm_pkgs_and_constraints (names, filter) ->
        let variables = OpamFilter.variables filter in
        let has_npm =
          let module V = OpamVariable in
          List.exists
            (fun full_var ->
              String.equal "npm-version"
                (V.to_string (V.Full.variable full_var)))
            variables
        in
        match has_npm with
        | false -> npm_pkgs_and_constraints
        | true -> (
            match filter with
            | OpamTypes.FOp (_a, `Eq, FString npm_constraint) ->
                (names, npm_constraint) :: npm_pkgs_and_constraints
            | _ ->
                print_endline
                  (Printf.sprintf
                     "Warning: package %s includes an invalid npm-version \
                      constraint which does not use equality in its formula: \
                      %s"
                     (OpamPackage.to_string nv)
                     (OpamFilter.to_string filter));
                npm_pkgs_and_constraints))
      []
      (OpamFile.OPAM.depexts opam)
  with Not_found -> []

let check_npm_deps cli =
  let doc = check_npm_deps_doc in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This command allows to read the current opam switch to find all \
         dependencies defining a depext belonging to \"npm\" platform and \
         their constraints, and then checks the `node_modules` folder to \
         verify the constraints are satisfied.";
      `P
        "This command only performs read operations, and does not install or \
         modify the opam switch or the `node_modules` folder in any way.";
      `S Manpage.s_arguments;
      `S Manpage.s_options;
    ]
    @ OpamArg.man_build_option_section
  in
  let check_npm_deps global_options build_options () =
    OpamArg.apply_global_options cli global_options;
    OpamArg.apply_build_options cli build_options;
    OpamClientConfig.update ~inplace_build:true ~working_dir:true ();
    (* Reducing log level, otherwise, some errors are triggered in CI:
       [WARNING] At /tmp/build_477fc1_dune/opam-25628-ddab92/default/packages/expect_test_helpers_kernel/expect_test_helpers_kernel.v0.9.0/opam:32:0-32:17::
       Unknown package flags deprecated ignored *)
    OpamCoreConfig.update ~verbose_level:0 ();
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamSwitchState.with_ `Lock_write gt @@ fun st ->
    let npm_depexts =
      List.filter_map
        (fun pkg ->
          let depexts = depexts pkg st.opams in
          match depexts with [] -> None | _ -> Some (pkg, depexts))
        OpamPackage.Set.(elements @@ st.installed)
    in
    let _DELETEME_matches =
      match_version_against_constraint "1.0.1" "^1.0.0"
    in
    let () =
      match npm_depexts with
      | [] -> ()
      | l ->
          print_endline "Found the following npm dependencies in opam files:";
          print_endline
            (OpamStd.List.concat_map " "
               (fun (opam_pkg, npm_pkgs_and_constraints) ->
                 String.concat "\n"
                   (List.map
                      (fun (npm_pkgs, npm_constraint) ->
                        Printf.sprintf
                          "opam pkg: %s, npm pkgs: %s, constraint: %s"
                          (OpamPackage.to_string opam_pkg)
                          (OpamSysPkg.Set.to_string npm_pkgs)
                          npm_constraint)
                      npm_pkgs_and_constraints))
               l)
    in
    OpamSwitchState.drop st
  in
  OpamArg.mk_command ~cli OpamArg.cli_original "opam-check-npm-deps" ~doc ~man
    Term.(
      const check_npm_deps $ OpamArg.global_options cli
      $ OpamArg.build_options cli)

[@@@ocaml.warning "-3"]

let () =
  OpamStd.Option.iter OpamVersion.set_git OpamGitVersion.version;
  OpamSystem.init ();
  OpamCliMain.main_catch_all @@ fun () ->
  match
    Term.eval ~catch:false (check_npm_deps (OpamCLIVersion.default, `Default))
  with
  | `Error _ -> exit (OpamStd.Sys.get_exit_code `Bad_arguments)
  | _ -> exit (OpamStd.Sys.get_exit_code `Success)
