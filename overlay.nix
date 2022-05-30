# Add all overlays
{ config, lib, pkgs, ... }:

let
  inherit (builtins) readDir;
  inherit (lib) mapAttrs' nameValuePair removeSuffix filterAttrs hasSuffix;
in {
  nixpkgs.overlays = [
    # Custom library functions
    (self: super: { lib = super.lib // { my = import ./lib.nix super.lib; }; })

    # Custom derivations
    # Generate a list of files in `packages` sub-directory, filter to only .nix files,
    # and then add each one to the overlay
    (self: super:
      super.lib.my.mapModules ./packages (p: super.callPackage p { }))
  ];
}
