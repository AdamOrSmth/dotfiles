{ config, lib, pkgs, ... }:

{
  # Options for my various modules
  my = {
    cli.nix = {
      autoGc = true;
      autoOptimise = true;
    };
    hardware.networking = { interface = "enp1s0"; };
    services = {
      caddy.enable = true;
      gitea.enable = true;
      navidrome = {
        enable = true;
        musicFolder = "${config.my.services.syncthing.dataDir}/Music";
      };
      openssh = {
        enable = true;
        key =
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIODhsvZT2xbdvgUOZn12BH1wy8slQAR2kRibdHy164bh <ssh://ad@5.161.48.126|ed25519>";
      };
      syncthing.enable = true;
    };
  };

  # Custom per-host configuration options

  # Hardware configuration options
  # (copy from /etc/nixos/hardware-configuration.nix)
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  boot.kernelPackages = pkgs.linuxPackages_5_15;

  boot.initrd.availableKernelModules =
    [ "ahci" "xhci_pci" "virtio_pci" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/sda1";
    fsType = "ext4";
  };

  swapDevices = [{ device = "/dev/sda2"; }];

  hardware.cpu.amd.updateMicrocode = true;

  # State version (copy from auto-generated configuration.nix during install)
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11";
}
