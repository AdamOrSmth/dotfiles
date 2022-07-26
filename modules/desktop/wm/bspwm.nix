path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
  configDir = "${config.my.configDir}";
in {
  options =
    setAttrByPath path { enable = mkEnableOption "`bspwm` as window manager"; };

  config = mkIf cfg.enable (mkMerge [{
    services = {
      xserver = {
        enable = true;
        windowManager.bspwm.enable = true;
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
      inherit (pkgs) alacritty dunst eww lightlocker playerctl polybar rofi;
    };

    my.home.configFiles = {
      "alacritty/alacritty.yml".source = "${configDir}/alacritty/alacritty.yml";
      "bspwm/bspwmrc".source = "${configDir}/bspwm/bspwmrc";
      "dunst/dunstrc".source = "${configDir}/bspwm/dunstrc";
      "polybar/config.ini".source = "${configDir}/bspwm/polybar.ini";
      "sxhkd/sxhkdrc".source = "${configDir}/bspwm/sxhkdrc";
    };
  }]);
}
