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
      default = {
        cp = "cp -iv";
        mv = "mv -iv";
        rm = "rm -iv";
        cat = "bat";
        la = "exa -al --color=always --group-directories-first --icons";
        ls = "exa -a --color=always --group-directories-first --icons";
        ll = "exa -l --color=always --group-directories-first --icons";
        lt = "exa -aT --color=always --group-directories-first --icons";
      };
    };
    path = mkOption {
      description = "List of additional directories to add to PATH";
      type = types.listOf types.path;
      default = [ binDir ];
    };
  };

  config = mkIf cfg.enable (mkMerge [{
    programs.fish = {
      enable = true;
      shellInit = cfg.extraInit + "\n"
        + "set -p ${lib.concatStringsSep " " cfg.path}";
      shellAbbrs = cfg.aliases;
    };
    users.defaultUserShell = pkgs.fish;
    environment.systemPackages =
      builtins.attrValues { inherit (pkgs) bat coreutils exa htop lsof; };
  }]);
}
