path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "standard Unix Password manager";
  };

  config = mkIf cfg.enable (mkMerge [{
    environment.systemPackages = [
      (pkgs.pass.withExtensions (exts:
        builtins.attrValues {
          inherit (exts) pass-audit pass-otp pass-update;
        }))
    ];
  }]);
}
