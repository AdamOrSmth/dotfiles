path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
  inherit (config.my) configDir;
in {
  options =
    setAttrByPath path { enable = mkEnableOption "`bspwm` as window manager"; };

  config = mkIf cfg.enable (mkMerge [{
    services = {
      xserver = {
        enable = true;
        windowManager.bspwm = {
          enable = true;
          configFile = "${configDir}/bspwm/bspwmrc";
          sxhkd.configFile = "${configDir}/sxhkd/sxhkdrc";
        };
      };

      picom = {
        enable = true;
        backend = "glx"; # Newer backend
        vSync = true;
        # Blur is hot
        settings = {
          blur = {
            method = "dual_kawase";
            strength = 3;
          };
        };
        # Blur requires experimental backends
        experimentalBackends = true;
      };
    };

    environment.systemPackages = builtins.attrValues {
      inherit (pkgs) alacritty dunst eww lightlocker polybar rofi;
    };
  }]);
}
