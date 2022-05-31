path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "sound services";
    bluetooth = mkEnableOption "Bluetooth-specific configuration";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      sound.enable = true;
      # One day, I should really switch to PipeWire. But today is not
      # that day.
      hardware.pulseaudio = {
        enable = true;
        package = pkgs.pulseaudioFull;
      };
    }
    (mkIf cfg.bluetooth {
      hardware.bluetooth = {
        enable = true;
        # Enable A2DP sink configuration
        settings.General.Enable = "Source,Sink,Media,Socket";
      };
    })
  ]);
}
