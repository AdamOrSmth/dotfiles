path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options =
    setAttrByPath path { enable = mkEnableOption "Starship shell prompt"; };

  config = mkIf cfg.enable (mkMerge [{
    programs.starship = {
      enable = true;
      settings = { };
    };
  }]);
}
