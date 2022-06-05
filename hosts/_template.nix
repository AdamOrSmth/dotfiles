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
      direnv.enable = false;
      doas.enable = false;
      exa.enable = false;
      files.enable = false;
      fish = {
        enable = false;
        extraInit = "";
        aliases = { };
      };
      git.enable = false;
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
      bspwm.enable = false;
      communication.enable = false;
      emacs.enable = false;
      gaming.enable = false;
      gimp.enable = false;
      multimedia.enable = false;
    };
    hardware = {
      filesystem.enable = false;
      mouse.enable = false;
      networking = {
        interface = null;
        wireless = false;
        mullvad = false;
      };
      pam-u2f.enable = false;
      sound = {
        enable = false;
        bluetooth = false;
      };
    };
    services = {
      caddy.enable = false;
      gitea.enable = false;
      lokinet.enable = false;
      navidrome = {
        enable = false;
        musicFolder = null;
      };
      onlykey.enable = false;
      openssh = {
        enable = false;
        user = null;
        key = null;
      };
      printing.enable = false;
      syncthing = {
        enable = false;
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
