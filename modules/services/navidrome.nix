path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "Navidrome server";
    musicFolder = mkOption {
      description = "Location of music folder";
      type = types.path;
    };
  };

  config = mkIf cfg.enable {
    services.navidrome = {
      enable = true;
      settings = { MusicFolder = cfg.musicFolder; };
    };

    # Override to fix Last.Fm scrobbling,
    # which requires read-access to certain folders in /etc
    systemd.services.navidrome.serviceConfig.BindReadOnlyPaths = [
      builtins.storeDir
      cfg.musicFolder
      "/etc/resolv.conf"
      "/etc/ssl/certs"
      "/etc/static/ssl/certs"
    ];
  };
}
