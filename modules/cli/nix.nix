path:

{ config, lib, pkgs, ... }:

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
      # Enable Flakes
      extraOptions = "experimental-features = nix-command flakes";

      # Automatically collect garbage
      gc = {
        automatic = cfg.autoGc;
        dates = "weekly";
      };

      # Automatically optimise store (deduplicate files via hard-linking)
      autoOptimiseStore = cfg.autoOptimise;
    };
  }]);
}
