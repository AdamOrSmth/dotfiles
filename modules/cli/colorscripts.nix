path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
  inherit (config.my) configDir;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "show shell-color-scripts on shell init";
  };

  config = mkIf cfg.enable (mkMerge [{
    environment.systemPackages = [ pkgs.my.shell-color-scripts ];
    my.fish.extraInit = "colorscript -r";
  }]);
}
