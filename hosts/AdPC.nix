{ config, lib, pkgs, ... }:

{
  # Options for my various modules
  my = {
    home.enable = true;
    hardware = {
      filesystem.enable = true;
      mouse.enable = true;
      networking = {
        interface = "enp37s0";
        mullvad = true;
      };
      sound = { enable = true; };
    };
    services = {
      lokinet.enable = true;
      onlykey.enable = true;
      printing.enable = true;
      syncthing = {
        enable = true;
        dataDir = "/home/ad/BigBoiStorage";
      };
    };
  };

  # Custom per-host configuration options
  services.xserver.videoDrivers = [ "nvidia" ];

  # Hardware configuration options
  # (copy from /etc/nixos/hardware-configuration.nix)
  boot.initrd.availableKernelModules =
    [ "nvme" "xhci_pci" "ahci" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/home/ad/BigBoiStorage" = {
    device = "/dev/disk/by-label/bigboistorage";
    fsType = "btrfs";
    options = [ "compress-force=zstd" "noatime" "nofail" ];
  };

  hardware.cpu.amd.updateMicrocode = true;

  # GRUB bootloader with UEFI and support for Windows dual-boot
  boot.loader = {
    efi.canTouchEfiVariables = true;
    grub = {
      enable = true;
      device = "nodev"; # Required for UEFI
      efiSupport = true;
      useOSProber = true;
    };
  };

  # State version (copy from auto-generated configuration.nix during install)
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05";
}