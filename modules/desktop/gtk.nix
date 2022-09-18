path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  inherit (config.my) configDir;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "GTK theme configuration using home-manager";
  };

  config = mkIf cfg.enable (mkMerge [{
    my.home.extraConfig.gtk = {
      enable = true;
      theme = {
        name = "Nordic";
        package = pkgs.nordic;
      };
    };
  }]);
}
