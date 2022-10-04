path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  inherit (config.my) configDir;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "self-hosted Anki sync server";
  };

  config = mkIf cfg.enable (mkMerge [{
    services.ankisyncd = {
      enable = true;
      port = 27702;
    };
    my.services.caddy.config = ''
      anki.adamorsomething.xyz {
        reverse_proxy 127.0.0.1:27702
      }
    '';
  }]);
}
