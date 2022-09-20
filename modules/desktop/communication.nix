path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
  inherit (pkgs) discord firejail;
  configDir = config.my.configDir;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "chat and communication programs";
  };

  config = mkIf cfg.enable (mkMerge [{
    environment.systemPackages = builtins.attrValues {
      inherit (pkgs) element-desktop session-desktop-appimage signal-desktop;
      inherit (pkgs.my) webcord;
    };

    my.home.configFiles."WebCord/Themes/nord".source =
      "${configDir}/webcord/nord.css";
  }]);
}
