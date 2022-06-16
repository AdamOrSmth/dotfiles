{ config, lib, pkgs, ... }:

{
  # Options for my various modules
  my = {
    home.enable = true;
    cli = {
      colorscripts.enable = true;
      direnv.enable = true;
      doas.enable = true;
      exa.enable = true;
      files.enable = true;
      fish.enable = true;
      git.enable = true;
      nix = {
        autoGc = true;
        autoOptimise = true;
      };
      pass.enable = true;
      restic.enable = true;
      starship.enable = true;
    };
    desktop = {
      browsers.enable = true;
      bspwm.enable = true;
      communication.enable = true;
      emacs.enable = true;
      gaming.enable = true;
      gimp.enable = true;
      hyprland.enable = true;
      multimedia.enable = true;
    };
    hardware = {
      filesystem.enable = true;
      mouse.enable = true;
      networking = {
        interface = "enp37s0";
        mullvad = true;
      };
      pam-u2f.enable = true;
      sound.enable = true;
    };
    services = {
      lokinet.enable = true;
      onlykey.enable = true;
      openssh = {
        enable = true;
        key =
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDfv959AiHhCkYxCvzA4nPY7xNJ3DSfK9XZxMaAhmuVC <ssh://ad@AdPC|ed25519>";
      };
      printing.enable = true;
      syncthing = {
        enable = true;
        dataDir = "/home/ad/BigBoiStorage";
      };
    };
  };

  # Custom per-host configuration options
  services.xserver.videoDrivers = [ "nvidia" ];
  boot.kernelPackages = pkgs.linuxPackages_zen;

  # Hardware configuration options
  # (copy from /etc/nixos/hardware-configuration.nix)
  boot.initrd.availableKernelModules =
    [ "nvme" "xhci_pci" "ahci" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules =
    [ "nvidia" "nvidia_modeset" "nvidia_uvm" "nvidia_drm" ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];
  boot.kernelParams = [ "rd.driver.blacklist=nouveau" "nvidia-drm.modeset=1" ];

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
