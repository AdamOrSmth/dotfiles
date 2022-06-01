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
    };
    aliases = mkOption {
      description = "Aliases for Fish";
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
  };

  config = mkIf cfg.enable (mkMerge [{
    programs.fish = {
      enable = true;
      shellInit = cfg.extraInit;
      shellAliases = cfg.aliases;
    };
    users.defaultUserShell = pkgs.fish;
    environment.systemPackages =
      builtins.attrValues { inherit (pkgs) bat coreutils exa htop lsof; };
  }]);
}
