path:

{ config, lib, pkgs, inputs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    autoGc = mkEnableOption "automatic Nix store garbage collection";
    autoOptimise = mkEnableOption "automatic optimisation of Nix store";
  };

  config = (mkMerge [{
    nix = {
      settings = {
        # Allow users of the wheel group to interact with the Nix daemon
        trusted-users = [ "@wheel" ];

        # Automatically optimise store (deduplicate files via hard-linking)
        auto-optimise-store = cfg.autoOptimise;
      };

      # Enable Flakes
      extraOptions = "experimental-features = nix-command flakes";
      # Pin `nixpkgs` in Flake registry to version used to build the system
      # (eliminates unnecessary dependencies)
      registry.nixpkgs.flake = inputs.nixpkgs;

      # Automatically collect garbage
      gc = {
        automatic = cfg.autoGc;
        dates = "weekly";
      };

    };

    environment.systemPackages = [ pkgs.nixfmt ];
  }]);
}
