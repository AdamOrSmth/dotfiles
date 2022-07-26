{ config, lib, pkgs, ... }:

{
  # Options for my various modules
  my = {
    user = {
      username = null;
      extraGroups = null;
    };
    home.enable = false;
    cli = {
      colorscripts.enable = false;
      direnv.enable = false;
      doas.enable = false;
      exa.enable = false;
      files.enable = false;
      fish = {
        enable = false;
        extraInit = "";
        abbrs = { };
        aliases = { };
      };
      git.enable = false;
      git-annex.enable = false;
      nix = {
        autoGc = false;
        autoOptimise = false;
      };
      pass.enable = false;
      restic.enable = false;
      starship.enable = false;
    };
    desktop = {
      browsers.enable = false;
      communication.enable = false;
      emacs.enable = false;
      gaming.enable = false;
      gimp.enable = false;
      gtk.enable = false;
      multimedia.enable = false;
      wm = {
        bspwm.enable = false;
        hyprland = {
          enable = false;
          nvidia = false;
          extraConfig = "";
        };
      };
    };
    hardware = {
      filesystem.enable = false;
      mouse.enable = false;
      networking = {
        interface = null;
        wireless = false;
        mullvad = false;
      };
      onlykey.enable = false;
      pam-u2f.enable = false;
      sound.enable = false;
    };
    services = {
      caddy.enable = false;
      git-hosting.enable = false;
      lokinet.enable = false;
      navidrome = {
        enable = false;
        musicFolder = null;
      };
      openssh = {
        enable = false;
        user = null;
        key = null;
      };
      printing.enable = false;
      syncthing = {
        enable = false;
        system = false;
        dataDir = null;
        enabledFolders = null;
        folderLocations = null;
      };
    };
  };

  # Custom per-host configuration options

  # Hardware configuration options
  # (copy from /etc/nixos/hardware-configuration.nix)

  # State version (copy from auto-generated configuration.nix during install)
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = null;
}
