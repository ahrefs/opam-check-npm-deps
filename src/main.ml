open Cmdliner

(* Inspired by https://gitlab.ocamlpro.com/louis/opam-custom-install *)

let check_npm_deps_doc = "Check for npm depexts inside the node_modules folder"

let match_version_against_constraint version formula =
  let open EsyPackageConfig.SemverVersion in
  let pf = Formula.parseExn formula in
  let pv = Version.parseExn version in
  Formula.DNF.matches ~version:pv pf

module Of_package_json = struct
  open EsyLib

  type t = { version : string [@default "0.0.0"] }
  [@@deriving of_yojson { strict = false }]

  let read path =
    let open RunAsync.Syntax in
    let* json = Fs.readJsonFile path in
    let* pkgJson = RunAsync.ofRun (Json.parseJsonWith of_yojson json) in
    return pkgJson.version
end

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
    let () =
      match npm_depexts with
      | [] -> ()
      | l ->
          List.iter
            (fun (opam_pkg, npm_pkgs_and_constraints) ->
              List.iter
                (fun (npm_pkgs, npm_constraint) ->
                  OpamSysPkg.Set.iter
                    (fun npm_pkg ->
                      let open EsyLib in
                      let path =
                        Path.(
                          currentPath () / "node_modules"
                          / OpamSysPkg.to_string npm_pkg
                          / "package.json")
                      in
                      let installed_version =
                        try Ok (RunAsync.runExn (Of_package_json.read path))
                        with _exn -> Error ()
                      in
                      match installed_version with
                      | Error () ->
                          print_endline
                            (Printf.sprintf
                               "Error: opam package \"%s\" requires npm package \"%s\" \
                                with constraint \"%s\", but file \"%s\" can not \
                                be found"
                               (OpamPackage.to_string opam_pkg)
                               (OpamSysPkg.to_string npm_pkg)
                               npm_constraint (Path.showNormalized path))
                      | Ok installed_version -> (
                          match
                            match_version_against_constraint installed_version
                              npm_constraint
                          with
                          | true ->
                              print_endline
                                (Printf.sprintf
                                   "Ok: opam package \"%s\" requires npm package: \"%s\" with \
                                    constraint \"%s\", version installed: \"%s\""
                                   (OpamPackage.to_string opam_pkg)
                                   (OpamSysPkg.to_string npm_pkg)
                                   npm_constraint installed_version)
                          | false ->
                              print_endline
                                (Printf.sprintf
                                   "Error: opam package \"%s\" requires npm \
                                    package \"%s\" with constraint \"%s\", but the \
                                    version installed is \"%s\""
                                   (OpamPackage.to_string opam_pkg)
                                   (OpamSysPkg.to_string npm_pkg)
                                   npm_constraint installed_version)))
                    npm_pkgs)
                npm_pkgs_and_constraints)
            l
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
