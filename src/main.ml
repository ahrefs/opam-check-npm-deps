open Cmdliner

(* Inspired by https://gitlab.ocamlpro.com/louis/opam-custom-install *)

let check_npm_deps_doc = "Check for npm depexts inside the node_modules folder"

let depexts nv opams =
  try
    let opam = OpamPackage.Map.find nv opams in
    List.fold_left
      (fun depexts (names, filter) ->
        let variables = OpamFilter.variables filter in
        let has_npm =
          let module V = OpamVariable in
          List.exists
            (fun full_var ->
              String.equal "npm-version"
                (V.to_string (V.Full.variable full_var)))
            variables
        in
        if has_npm then OpamSysPkg.Set.Op.(names ++ depexts) else depexts)
      OpamSysPkg.Set.empty
      (OpamFile.OPAM.depexts opam)
  with Not_found -> OpamSysPkg.Set.empty

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
    (* OpamCoreConfig.update ~verbose_level:6 (); *)
    print_endline
      ("VERBOSE 2: " ^ string_of_int OpamCoreConfig.(!r.verbose_level));
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamSwitchState.with_ `Lock_write gt @@ fun st ->
    let npm_depexts =
      List.filter_map
        (fun pkg ->
          let depexts = depexts pkg st.opams in
          match OpamSysPkg.Set.cardinal depexts with
          | 0 -> None
          | _ -> Some (pkg, depexts))
        OpamPackage.Set.(elements @@ st.installed)
    in
    let () =
      match npm_depexts with
      | [] -> ()
      | l ->
          print_endline
            ("VERBOSE 3: " ^ string_of_int OpamCoreConfig.(!r.verbose_level));
          print_endline "Found the following npm dependencies in opam files:";
          print_endline
            (OpamStd.List.concat_map " "
               (fun (pkg, npm_deps) ->
                 Printf.sprintf "pkg: %s, depexts: %s\n"
                   (OpamPackage.to_string pkg)
                   (OpamSysPkg.Set.to_string npm_deps))
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
  (* OpamCoreConfig.update ~verbose_level:6 (); *)
  OpamStd.Option.iter OpamVersion.set_git OpamGitVersion.version;
  OpamSystem.init ();
  print_endline ("VERBOSE 1: " ^ string_of_int OpamCoreConfig.(!r.verbose_level));
  OpamCliMain.main_catch_all @@ fun () ->
  match
    Term.eval ~catch:false (check_npm_deps (OpamCLIVersion.default, `Default))
  with
  | `Error _ -> exit (OpamStd.Sys.get_exit_code `Bad_arguments)
  | _ -> exit (OpamStd.Sys.get_exit_code `Success)
