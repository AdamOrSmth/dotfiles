path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
  inherit (config.my) configDir;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "Hyprland Wayland compositor";
    nvidia = mkEnableOption "NVIDIA-specific configurations";
  };

  config = mkIf cfg.enable (mkMerge [{
    programs.hyprland = {
      enable = true;
      extraPackages = builtins.attrValues {
        inherit (pkgs)
          alacritty rofi-wayland swaybg swaylock-effects wl-clipboard;
        inherit (pkgs.libsForQt5.qt5) qtwayland;
      };
    };

    xdg.portal = {
      enable = true;
      gtkUsePortal = true;
      # Includes fix for Hyprland, need to override the default package set by the module
      extraPortals = lib.mkForce [ pkgs.my.xdg-desktop-portal-wlr-hyprland ];
    };

    environment.variables = {
      MOZ_ENABLE_WAYLAND = "1";
      SDL_VIDEODRIVER = "wayland";
    } // lib.mkIf cfg.nvidia {
      GBM_BACKEND = "nvidia-drm";
      __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      WLR_NO_HARDWARE_CURSORS = "1";
    };

    # Required by swaylock
    security.pam.services.swaylock = { };
  }]);
}