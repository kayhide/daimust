
{ nixpkgs ? null
, compiler ? "ghc844"
}:

let
  nixpkgsSrc =
    if isNull nixpkgs
      then
        builtins.fetchTarball {
          # Recent version of nixpkgs-18.09 as of 2019-04-01.
          url = "https://github.com/NixOS/nixpkgs/archive/3eed6d45739bfb6ef4e74837199021fe129a1f1f.tar.gz";
          sha256 = "068zwvlhizayrz6hhkqhl0p9w97wqsi4yfphgq17p4xcvf997nw8";
        }
      else
        nixpkgs;
  pkgs = import nixpkgsSrc {};
in

with pkgs;

let
  diamust = haskell.packages.${compiler}.developPackage {
    root = ./.;
    modifier = drv: haskell.lib.overrideCabal drv (oldAttrs: {
      doCheck = false;
    });
  };
in

diamust
