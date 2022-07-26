path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  inherit (config.my) configDir;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    enable = mkEnableOption throw "Create some options here!";
    field = mkOption {
      description = throw "Create some options here!";
      type = types.anything;
      default = throw "Add a default value here (if needed)";
    };
  };

  config =
    mkIf cfg.enable (mkMerge [{ setting = throw "Set some options here!"; }]);
}
