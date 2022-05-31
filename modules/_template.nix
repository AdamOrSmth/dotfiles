path:

{ config, lib, pkgs, ... }:

let
  inherit (lib) getAttrFromPath setAttrByPath mkEnableOption mkIf;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    enable = mkEnableOption throw "Create some options here!";
  };

  config = mkIf cfg.enable { setting = throw "Set some options here!"; };
}
