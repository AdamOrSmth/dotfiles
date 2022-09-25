path:

{ config, lib, pkgs, inputs, system, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
  inherit (config.my) configDir;
  hyprlandPkgs = inputs.hyprland.packages.${system};
in {
  options = setAttrByPath path {
    enable = mkEnableOption "Hyprland Wayland compositor";
    nvidia = mkEnableOption "NVIDIA-specific configurations";
    extraConfig = mkOption {
      description = "Extra device-specific lines to add to configuration";
      type = types.lines;
      default = "";
    };
  };

  config = mkIf cfg.enable (mkMerge [{
    programs.hyprland = {
      enable = true;
      package = hyprlandPkgs.default.override {
        nvidiaPatches = cfg.nvidia;
        wlroots = if cfg.nvidia then
          hyprlandPkgs.wlroots-hyprland.overrideAttrs (oldAttrs: {
            patches = oldAttrs.patches
              # Yes, this file should go somewhere else, but whatever
              ++ [ ./_wlroots-nvidia-screenshare.patch ];
          })
        else
          hyprlandPkgs.wlroots-hyprland;
      };
    };
    environment.systemPackages = builtins.attrValues {
      inherit (pkgs)
        alacritty grim mako pcmanfm playerctl rofi-wayland slurp swaybg
        swaylock-effects wl-clipboard;
      inherit (pkgs.xorg) xeyes;
    };

    # Start Hyprland + set relevant environment variables
    my.cli.fish.extraInit = let
      nvidiaExtras = lib.optionalString cfg.nvidia ''
        export GBM_BACKEND=nvidia-drm
        export __GLX_VENDOR_LIBRARY_NAME=nvidia
        export WLR_NO_HARDWARE_CURSORS=1
        export LIBVA_DRIVER_NAME=nvidia
      '';
    in ''
      if test (tty) = /dev/tty1
        ${nvidiaExtras}
        export SDL_VIDEODRIVER=wayland
        exec Hyprland
      end
    '';

    # Required by swaylock
    security.pam.services.swaylock = { };

    my.home.configFiles = {
      "hypr/hyprland.conf".source = "${configDir}/hyprland/hyprland.conf";
      "hypr/extra.conf".text = cfg.extraConfig;
      "alacritty/alacritty.yml".source = "${configDir}/alacritty/alacritty.yml";
      "mako/config".source = "${configDir}/hyprland/mako";
      "rofi".source = "${configDir}/rofi";
    };
  }]);
}
