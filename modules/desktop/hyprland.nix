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

    # Rest of settings set by Hyprland module
    # https://github.com/vaxerski/Hyprland/blob/main/nix/module.nix#L59
    xdg.portal.gtkUsePortal = true;

    environment.systemPackages = builtins.attrValues {
      inherit (pkgs) alacritty rofi-wayland swaybg swaylock wl-clipboard;
      inherit (pkgs.qt6) qtwayland;
    };
  }]);
}
