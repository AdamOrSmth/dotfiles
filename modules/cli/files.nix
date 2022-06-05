path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
  inherit (config.my) configDir;
in {
  options =
    setAttrByPath path { enable = mkEnableOption "file-management CLI tools"; };

  config = mkIf cfg.enable (mkMerge [{
    environment.systemPackages =
      builtins.attrValues { inherit (pkgs) file gocryptfs unzip zip; };
  }]);
}
