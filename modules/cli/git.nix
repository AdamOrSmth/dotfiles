path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
  inherit (config.my) configDir;
in {
  options = setAttrByPath path { enable = mkEnableOption "Git configuration"; };

  config = mkIf cfg.enable (mkMerge [{
    environment.systemPackages = builtins.attrValues {
      inherit (pkgs)
        git git-crypt git-annex git-annex-remote-rclone
        git-annex-remote-googledrive git-remote-gcrypt;
    };
    my.home.configFiles."git/config".source = "${configDir}/git/config";
  }]);
}
