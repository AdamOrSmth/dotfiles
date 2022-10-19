path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    enable =
      mkEnableOption "Steam, Lutris, and other gaming-related applications";
  };

  config = mkIf cfg.enable (mkMerge [{
    programs = {
      steam.enable = true;
      gamemode.enable = true;
    };
    environment.systemPackages = builtins.attrValues {
      inherit (pkgs) lutris;
      inherit (pkgs.my) tetrio-desktop;
    };
  }]);
}
