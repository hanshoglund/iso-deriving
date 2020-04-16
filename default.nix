
let
  opts = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          # No overrides
        };
      };
    };
  };

  pkgs = import (builtins.fetchTarball {
    url = https://github.com/nixos/nixpkgs/archive/56f7c93a2fcd325506a6bfa7bf8e9faa2a2c7530.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0fm9a24kjailx9qlyvwwa3a635m8pdk00329rldc541zhpmrklib";
  }) { config = opts; };
in

pkgs.stdenv.mkDerivation {
  name = "iso-deriving";
  buildInputs = [
    (
    pkgs.haskellPackages.ghcWithPackages (pkgs:
        [ pkgs.cabal-install
        ])
    )
  ];
}

