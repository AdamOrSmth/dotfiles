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
          alacritty bemenu dunst grim pcmanfm playerctl slurp swaybg
          swaylock-effects wl-clipboard wofi;
        inherit (pkgs.xorg) xeyes;
      };
    };

    # Actually start the darn thing + set relevant environment variables
    # What the bloody hell is this? Well, Fish's `exec` command doesn't
    # inherit its environment, but Bash's does (according to my testing),
    # so we have to start a Bash instance in order for Hyprland to inherit
    # our variables.
    #services.xserver.displayManager.lightdm.enable = false;
    my.cli.fish.extraInit = let
      nvidiaExtras = lib.optionalString cfg.nvidia
        "GBM_BACKEND=nvidia-drm __GLX_VENDOR_LIBRARY_NAME=nvidia WLR_NO_HARDWARE_CURSORS=1";
      command =
        "MOZ_ENABLE_WAYLAND=1 SDL_VIDEODRIVER=wayland QT_QPA_PLAYFORM=wayland ${nvidiaExtras} exec Hyprland";
    in ''
      if test (tty) = /dev/tty1
        exec bash -c '${command}'
      end
    '';

    # Includes fix for Hyprland, need to override the default package set by the module
    xdg.portal.extraPortals =
      lib.mkForce [ pkgs.my.xdg-desktop-portal-wlr-hyprland ];

    # Required by swaylock
    security.pam.services.swaylock = { };

    my.home.configFiles."hypr/hyprland.conf".source =
      "${configDir}/hyprland/hyprland.conf";
  }]);
}
