path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path { enable = mkEnableOption "GIMP editor"; };

  config = mkIf cfg.enable (mkMerge [{
    environment.systemPackages = [
      (pkgs.gimp-with-plugins.override {
        plugins = [ pkgs.gimpPlugins.resynthesizer ];
      })
    ];
  }]);
}
