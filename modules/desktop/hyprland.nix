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
  };

  config = mkIf cfg.enable (mkMerge [{
    programs.hyprland = {
      enable = true;
      # Prefer to manage packages myself
      extraPackages = [ ];
    };
    xdg.portal = {
      enable = true;
      wlr.enable = true;
      extraPortals = builtins.attrValues {
        inherit (pkgs) xdg-desktop-portal-wlr xdg-desktop-portal-gtk;
      };
      gtkUsePortal = true;
    };

    environment.systemPackages = builtins.attrValues {
      inherit (pkgs) alacritty rofi-wayland wl-clipboard;
      inherit (pkgs.qt6) qtwayland;
    };
  }]);
}
