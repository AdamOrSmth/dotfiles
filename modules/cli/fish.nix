path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "Fish as default shell";
    extraInit = mkOption {
      description = "Extra lines to add to Fish configuration";
      type = types.lines;
      default = "";
    };
    aliases = mkOption {
      description = "Aliases for Fish";
      type = types.attrsOf types.nonEmptyStr;
      default = { };
    };
  };

  config = mkIf cfg.enable (mkMerge [{
    programs.fish = {
      enable = true;
      shellInit = cfg.extraInit;
      shellAliases = cfg.aliases;
    };
    users.defaultUserShell = pkgs.fish;
  }]);
}
