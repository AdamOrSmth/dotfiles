path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "`doas` as `sudo` replacement";
  };

  config = mkIf cfg.enable (mkMerge [{
    security = {
      sudo.enable = false;
      doas = {
        enable = true;
        extraRules = [{
          users = [ config.my.user.username ];
          keepEnv = true;
          persist = true;
        }];
      };
    };
    my.cli.fish.abbrs.sudo = "doas";
  }]);
}
