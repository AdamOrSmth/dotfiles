path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
  inherit (config.my) binDir;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "Fish as default shell";
    extraInit = mkOption {
      description = "Extra lines to add to Fish configuration";
      type = types.lines;
    };
    aliases = mkOption {
      description = "Abbreviations for Fish";
      type = types.attrsOf types.nonEmptyStr;
      default = { };
    };
    path = mkOption {
      description = "List of additional directories to add to PATH";
      type = types.listOf types.path;
      default = [ ];
    };
  };

  config = mkIf cfg.enable (mkMerge [{
    programs.fish = {
      enable = true;
      shellInit = ''
        ${cfg.extraInit}
        set -U fish_greeting
        fish_add_path -P ${lib.concatStringsSep " " ([ binDir ] ++ cfg.path)}
      '';
      shellAbbrs = {
        cp = "cp -iv";
        mv = "mv -iv";
        rm = "rm -iv";
        cat = "bat";
      } // cfg.aliases;
    };
    users.defaultUserShell = pkgs.fish;
    environment.systemPackages =
      builtins.attrValues { inherit (pkgs) bat coreutils htop lsof; };
  }]);
}
