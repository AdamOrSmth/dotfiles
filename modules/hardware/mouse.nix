path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "mouse-specific (libinput) options";
  };

  config = mkIf cfg.enable (mkMerge [{
    services.xserver.libinput = {
      enable = true;
      mouse = {
        # I have yet to find good mouse accel on Linux, something
        # akin to RawAccel would be amazing.
        accelProfile = "flat";
        accelSpeed = "0";
      };

      # Make scrolling up on touchpad scroll content down. I'm
      # sure some people have strong opinions on this, but
      # this is just what's naturally comfortable for me,
      # to each their own.
      touchpad.naturalScrolling = true;
    };
  }]);
}
