{ config, lib, pkgs, ... }:

let inherit (lib) mkEnableOption mkIf;
in {
  options.my.hardware.filesystem.enable =
    mkEnableOption "root file system management using labels";

  config = mkIf config.my.hardware.filesystem.enable {
    fileSystems = {
      "/" = {
        device = "/dev/disk/by-label/root";
        fsType = "btrfs";
        options = [ "compress-force=zstd" "ssd" "noatime" ];
      };

      "/boot" = {
        device = "/dev/disk/by-label/boot";
        fsType = "vfat";
      };
    };
  };
}
