
{ nixpkgs ? null
, compiler ? "ghc864"
}:

let
  nixpkgsSrc =
    if isNull nixpkgs
      then
        builtins.fetchTarball {
          # Recent version of nixpkgs-19.03 as of 2019-06-26.
          url = "https://github.com/NixOS/nixpkgs/archive/8634c3b619909db7fc747faf8c03592986626e21.tar.gz";
          sha256 = "sha256:0hcpy4q64vbqmlmnfcavfxilyygyzpwdsss8g3p73ikpic0j6ziq";
        }
      else
        nixpkgs;

  daimustOverlay = self: super: {
    haskell = super.haskell // {
      # Add a daimust attribute for every haskell package set.
      packageOverrides = hself: hsuper:
        super.haskell.packageOverrides hself hsuper // {
          daimust = hself.developPackage {
            name = "daimust";
            root =
              builtins.filterSource
                (path: type: with self.stdenv.lib;
                  ! elem (baseNameOf path) [ ".git" "result" ".stack-work" ] &&
                  ! any (flip hasPrefix (baseNameOf path)) [ "dist" ".ghc" ]
                )
                ./.;
          };
        };
    };

    daimust =
      self.haskell.lib.justStaticExecutables
        self.haskell.packages.${compiler}.daimust;
  };

  pkgs = import nixpkgsSrc { overlays = [daimustOverlay]; };
in

pkgs.daimust
