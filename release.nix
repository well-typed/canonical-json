let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc822.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          canonical-json  = haskellPackagesNew.callPackage ./canonical-json.nix { };
        };
      };
    };
  };

  # pinning
  fetchNixPkgs  = import ./fetch-nixpkgs.nix;
  pkgs          = import fetchNixPkgs { inherit config; };

in
  { canonical-json = pkgs.haskellPackages.canonical-json;
  }
