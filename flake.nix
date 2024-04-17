{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        packages.default = pkgs.ocamlPackages.buildDunePackage {
          pname = "opam-repo-ci-diff";
          version = "n/a";
          src = ./.;
          propagatedBuildInputs = with pkgs.ocamlPackages; [
            cmdliner
            curly
            lambdasoup
            re
          ];
        };
      });
}
