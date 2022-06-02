path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options =
    setAttrByPath path { enable = mkEnableOption "pam-u2f authentication"; };

  config = mkIf cfg.enable (mkMerge [{
    security.pam.u2f = {
      enable = true;
      cue = true;
    };
  }]);
}
