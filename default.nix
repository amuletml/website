let
  rev = "0b97a9c4755ee71e64bac9408c766f13a930999a";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "030knsmkqy7fhpi8dsxhm76lvhak551bvzicgsnjidhwv063bw32";
  };
  nixpkgs = import pkgs {
    config = {
    packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {
            hakyll   = pkgs.haskell.lib.doJailbreak haskellPackagesOld.hakyll;
            lrucache = pkgs.haskell.lib.doJailbreak haskellPackagesOld.lrucache;

            ghc861 = pkgs.haskell.packages.ghc861.override {
              overrides = self: super: {
                io-capture = pkgs.haskell.lib.overrideCabal super.io-capture (old: rec {
                  doCheck = false;
                });
              };
            };
          };
        };
      };
    };
  };
in { compiler ? "ghc861", ci ? false }:

let
  inherit (nixpkgs) pkgs haskell;

  f = { mkDerivation, stdenv
      , base
      , data-default
      , hakyll
      , hakyll-sass
      , hsass
      , tagsoup
      , text
      , skylighting
      , unordered-containers
      }:
      mkDerivation rec {
        pname = "amuletml-web";
        version = "0.1.0.0";
        src = ./.;

        isLibrary = false;
        isExecutable = true;

        executableHaskellDepends = [
          base data-default hakyll hakyll-sass hsass tagsoup
          text skylighting unordered-containers
        ];

        buildDepends = [ pkgs.cabal-install ];

        homepage = "https://amulet.ml";
        description = "A functional programming language";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = pkgs.haskell.packages.${compiler};

  drv = nixpkgs.haskellPackages.callPackage f {};

in
  if pkgs.lib.inNixShell then drv.env else drv
