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
      settings = {
        MusicFolder = cfg.musicFolder;
        CoverArtPriority = "cover.*, embedded";
      };
    };
    my.services.caddy.config = ''
      music.adamorsomething.xyz {
        reverse_proxy 127.0.0.1:${
          toString config.services.navidrome.settings.Port
        }
      }
    '';
  };
}
