# Add all overlays
{ config, lib, pkgs, ... }:

let
  inherit (builtins) readDir;
  inherit (lib) mapAttrs' nameValuePair removeSuffix filterAttrs hasSuffix;
in {
  nixpkgs.overlays = [
    # Custom derivations
    # Generate a list of files in `packages` sub-directory, filter to only .nix files,
    # and then add each one to the overlay
    (self: super:
      let
        files = readDir ./packages;
        nixFiles = filterAttrs (n: _: hasSuffix ".nix" n) files;
      in mapAttrs' (n: _:
        nameValuePair (removeSuffix ".nix" n)
        (super.callPackage (import ./packages/${n}) { })) nixFiles)
  ];
}
