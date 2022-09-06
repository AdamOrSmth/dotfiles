path:

{ config, lib, pkgs, inputs, system, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
  inherit (config.my) configDir;
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
      package = inputs.hyprland.packages.${system}.default.override {
        nvidiaPatches = cfg.nvidia;
      };
    };
    environment.systemPackages = builtins.attrValues {
      inherit (pkgs)
        alacritty bemenu dunst grim pcmanfm playerctl slurp swaybg
        swaylock-effects wl-clipboard wofi;
      inherit (pkgs.xorg) xeyes;
    };

    # Actually start the darn thing + set relevant environment variables
    # What the bloody hell is this? Well, Fish's `exec` command doesn't
    # inherit its environment, but Bash's does (according to my testing),
    # so we have to start a Bash instance in order for Hyprland to inherit
    # our variables.
    #services.xserver.displayManager.lightdm.enable = false;
    my.cli.fish.extraInit = let
      nvidiaExtras = lib.optionalString cfg.nvidia
        "GBM_BACKEND=nvidia-drm __GLX_VENDOR_LIBRARY_NAME=nvidia WLR_NO_HARDWARE_CURSORS=1 WLR_BACKEND=vulkan LIBVA_DRIVER_NAME=nvidia CLUTTER_BACKEND=wayland QT_WAYLAND_DISABLE_WINDOWDECORATION=1";
      command =
        "MOZ_ENABLE_WAYLAND=1 SDL_VIDEODRIVER=wayland QT_QPA_PLAYFORM=wayland XDG_SESSION_TYPE=wayland ${nvidiaExtras} exec Hyprland";
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

    my.home.configFiles = {
      "hypr/hyprland.conf".source = "${configDir}/hyprland/hyprland.conf";
      "hypr/extra.conf".text = cfg.extraConfig;
      "alacritty/alacritty.yml".source = "${configDir}/alacritty/alacritty.yml";
    };
  }]);
}
