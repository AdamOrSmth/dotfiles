path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  inherit (config.my) configDir;
  cfg = getAttrFromPath path config;
in {
  options =
    setAttrByPath path { enable = mkEnableOption "git-annex packages"; };

  config = mkIf cfg.enable (mkMerge [{
    environment.systemPackages = builtins.attrValues {
      inherit (pkgs)
        git-annex git-annex-remote-rclone git-annex-remote-googledrive
        git-remote-gcrypt rsync rclone;
    };
  }]);
}
