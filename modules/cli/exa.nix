path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
  inherit (config.my) configDir;
in {
  options =
    setAttrByPath path { enable = mkEnableOption "exa as `ls` replacement"; };

  config = mkIf cfg.enable (mkMerge [{
    environment.systemPackages = [ pkgs.exa ];
    my.cli.fish.aliases = {
      la = "exa -al --color=always --group-directories-first --icons";
      ls = "exa -a --color=always --group-directories-first --icons";
      ll = "exa -l --color=always --group-directories-first --icons";
      lt = "exa -aT --color=always --group-directories-first --icons";
    };
  }]);
}
