let
fixed_nixpkgs = builtins.fetchTarball {
      name = "nixpkgs-unstable-2025-04";
      url = "https://github.com/nixos/nixpkgs/archive/ab8b04cdce6a103c76cac8966fbfa9de37823653.tar.gz";
      sha256 = "1igk8vya65yc3wcksyacbx7r2wk3vipg23q5y4ifpvd1r4r21bmf";
    };
in

{ nixpkgs ? fixed_nixpkgs,
  system ? builtins.currentSystem }:
with (import nixpkgs { inherit system; });

let
  mkDerivation =
    { srcs ? ./elm-srcs.nix
    , src
    , name
    , srcdir ? "./src"
    , registryDat ? ./registry.dat
    }:
    stdenv.mkDerivation {
      inherit name src;

      buildInputs = [ elmPackages.elm elmPackages.elm-land ];

      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import srcs;
        elmVersion = "0.19.1";
        inherit registryDat;
      };

      installPhase = ''
        elm-land build
        mkdir -p $out
        cp -r dist/* $out
      '';
    };
in mkDerivation {
  name = "magsview";
  srcs = ./elm-srcs.nix;
  src = builtins.filterSource
            (path: _type: baseNameOf path != ".git" && baseNameOf path != "result")
            ./.;
  srcdir = "./src";
}

