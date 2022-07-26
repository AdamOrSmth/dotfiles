path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
  inherit (config.my) configDir;
in {
  options =
    setAttrByPath path { enable = mkEnableOption "multimedia applications"; };

  config = mkIf cfg.enable (mkMerge [{
    environment.systemPackages = builtins.attrValues {
      inherit (pkgs)
        anki-bin calibre drawio feh flameshot glava jabref keepassxc libreoffice
        mpv obs-studio pavucontrol qbittorrent sonixd tenacity;
      inherit (pkgs.gnome) simple-scan;
    };
  }]);
}
