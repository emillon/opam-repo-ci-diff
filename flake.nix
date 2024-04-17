{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = final: prev: {
          ocamlPackages = prev.ocamlPackages.overrideScope' (ofinal: oprev: {
            curly = oprev.curly.overrideAttrs (oldAttrs:
              let version = "0.3.0";
              in {
                inherit version;
                src = prev.fetchurl {
                  url =
                    "https://github.com/rgrinberg/curly/releases/download/${version}/curly-${version}.tbz";
                  hash = "sha256-Qn/PKBNOcMt3dk2f7uJD8x0yo4RHobXSjTQck7fcXTw=";
                };
              });
          });
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
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
