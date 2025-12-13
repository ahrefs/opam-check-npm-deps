open OpamCmdliner

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

  let read_version path =
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
                Printf.printf
                  "Warning: package %s includes an invalid npm-version \
                   constraint which does not use equality in its formula: %s.\n\
                   To fix the issue, use an equality formula, e.g. \
                   {npm-version = \"^1.0.0\"}\n"
                  (OpamPackage.to_string nv)
                  (OpamFilter.to_string filter);
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
  let dry_run =
    Arg.(
      value & flag
      & info [ "dry-run"; "d" ]
          ~doc:
            "Check `npm-version` constraints against installed npm packages, \
             but don't have the command return any error status code.")
  in
  let check_npm_deps dry_run () =
    let error_found = ref false in
    OpamClientConfig.opam_init ();
    OpamClientConfig.update ~inplace_build:true ~working_dir:true ();
    (* Reducing log level, otherwise, some errors are triggered in CI:
       [WARNING] At /tmp/build_477fc1_dune/opam-25628-ddab92/default/packages/expect_test_helpers_kernel/expect_test_helpers_kernel.v0.9.0/opam:32:0-32:17::
       Unknown package flags deprecated ignored *)
    OpamCoreConfig.update ~verbose_level:0 ();
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamSwitchState.with_ `Lock_none gt @@ fun st ->
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
                        try
                          Ok
                            (RunAsync.runExn
                               (Of_package_json.read_version path))
                        with _exn -> Error ()
                      in
                      match installed_version with
                      | Error () ->
                          error_found := true;
                          Printf.eprintf
                            "Error: opam package \"%s\" requires npm package \
                             \"%s\" with constraint \"%s\", but file \"%s\" \
                             can not be found\n"
                            (OpamPackage.to_string opam_pkg)
                            (OpamSysPkg.to_string npm_pkg)
                            npm_constraint (Path.showNormalized path)
                      | Ok installed_version -> (
                          match
                            match_version_against_constraint installed_version
                              npm_constraint
                          with
                          | true ->
                              Printf.printf
                                "Ok: opam package \"%s\" requires npm package: \
                                 \"%s\" with constraint \"%s\", version \
                                 installed: \"%s\"\n"
                                (OpamPackage.to_string opam_pkg)
                                (OpamSysPkg.to_string npm_pkg)
                                npm_constraint installed_version
                          | false ->
                              error_found := true;
                              Printf.eprintf
                                "Error: opam package \"%s\" requires npm \
                                 package \"%s\" with constraint \"%s\", but \
                                 the version installed found in file \"%s\" is \
                                 \"%s\"\n"
                                (OpamPackage.to_string opam_pkg)
                                (OpamSysPkg.to_string npm_pkg)
                                npm_constraint (Path.showNormalized path)
                                installed_version))
                    npm_pkgs)
                npm_pkgs_and_constraints)
            l
    in
    OpamSwitchState.drop st;
    match !error_found && not dry_run with
    | false -> ()
    | true -> exit (OpamStd.Sys.get_exit_code `False)
  in
  OpamArg.mk_command ~cli OpamArg.cli_original "opam-check-npm-deps" ~doc ~man
    Term.(const check_npm_deps $ dry_run)

[@@@ocaml.warning "-3"]

let () =
  Stdlib.Option.iter OpamVersion.set_git OpamGitVersion.version;
  OpamSystem.init ();
  OpamCliMain.main_catch_all @@ fun () ->
  (* TODO: Get rid of this whenever https://github.com/dbuenzli/cmdliner/pull/161 is available *)
  let to_new_cmdliner_api (term, info) = Cmd.v info term in
  let command =
    to_new_cmdliner_api (check_npm_deps (OpamCLIVersion.default, `Default))
  in
  match Cmd.eval_value ~catch:false command with
  | Error _ -> exit (OpamStd.Sys.get_exit_code `Bad_arguments)
  | Ok _ -> exit (OpamStd.Sys.get_exit_code `Success)
