{ config, lib, pkgs, ... }:

{
  # Options for my various modules
  my = {
    home.enable = true;
    cli = {
      direnv.enable = true;
      fish = { enable = true; };
      nix = {
        autoGc = true;
        autoOptimise = true;
      };
      starship.enable = true;
    };
    hardware = {
      filesystem.enable = true;
      mouse.enable = true;
      networking = {
        interface = "wlp1s0";
        wireless = true;
        mullvad = true;
      };
      sound = {
        enable = true;
        bluetooth = true;
      };
    };
    services = {
      lokinet.enable = true;
      onlykey.enable = true;
      syncthing = {
        enable = true;
        enabledFolders = [ "pass" "sync" ];
        folderLocations = {
          pass = "/home/ad/Pass";
          sync = "/home/ad/ST";
        };
      };
    };
  };

  # Custom per-host configuration options

  # Hardware configuration options
  # (copy from /etc/nixos/hardware-configuration.nix)
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/2fb72c4b-8950-4a43-bf13-f5a2d1bbfa39";
      preLVM = true;
      allowDiscards = true;
    };
  };

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  powerManagement.cpuFreqGovernor = "powersave";

  swapDevices = [{ device = "/swapfile"; }];
  boot = {
    kernelParams = [ "resume_offset=6070817" ];
    resumeDevice = "/dev/disk/by-label/root";
  };

  hardware.cpu.intel.updateMicrocode = true;

  # State version (copy from auto-generated configuration.nix during install)
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11";
}
