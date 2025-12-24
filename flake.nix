{
  description = "Reason React Nix Flake";

  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
  };

  outputs = { self, nixpkgs }:
    let
      forAllSystems = f: nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (system:
        let
          pkgs = nixpkgs.legacyPackages.${system}.extend (self: super: {
            ocamlPackages = super.ocaml-ng.ocamlPackages_5_3;
          });
        in
        f pkgs);
    in
    {
      packages = forAllSystems (pkgs:
        let
          packages = with pkgs.ocamlPackages; {
            opam-check-npm-deps = buildDunePackage {
              pname = "opam-check-npm-deps";
              version = "n/a";

              src =
                let fs = pkgs.lib.fileset; in
                fs.toSource {
                  root = ./.;
                  fileset = fs.unions [
                    ./dune-project
                    ./opam-check-npm-deps.opam
                    ./src
                    ./test
                  ];
                };

              doCheck = false; # we don't initialize OPAM
              propagatedBuildInputs = [
                opam-client
                lwt
                angstrom
                fmt
                bos
                yojson
              ];
            };
          };
        in
        packages // { default = packages.opam-check-npm-deps; });

      devShells = forAllSystems (pkgs:
        let
          makeDevShell = { packages, release-mode ? false }:
            pkgs.mkShell {
              inputsFrom = pkgs.lib.attrValues packages;
              nativeBuildInputs =
                with pkgs.ocamlPackages; [ ocamlformat pkgs.nodejs_24 ocaml dune ]
                  ++ pkgs.lib.optionals release-mode (with pkgs; [
                  cacert
                  curl
                  ocamlPackages.dune-release
                  ocamlPackages.odoc
                  git
                ]);
            };
          packages = self.packages.${pkgs.stdenv.hostPlatform.system};
        in
        {
          default = makeDevShell { inherit packages; };
          release = makeDevShell {
            inherit packages;
            release-mode = true;
          };
        });
    };
}
