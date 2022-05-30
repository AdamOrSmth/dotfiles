path:

{ config, lib, pkgs, ... }:

let inherit (lib) setAttrByPath mkEnableOption getAttrFromPath mkIf;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "root file system management using labels";
  };

  config = mkIf (getAttrFromPath path config).enable {
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
