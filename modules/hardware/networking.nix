path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    interface = mkOption {
      description = "Network interface to enable DHCP on";
      type = types.nonEmptyStr;
    };
    wireless = mkEnableOption "Wi-Fi support";
    mullvad = mkEnableOption "Mullvad VPN";
  };

  config = (mkMerge [
    {
      networking = {
        useDHCP = false; # Deprecated flag
        interfaces.${cfg.interface}.useDHCP = true;
        networkmanager.enable = cfg.wireless;
      };
    }
    (mkIf cfg.mullvad {
      services.mullvad-vpn.enable = true;
      environment.systemPackages = [ pkgs.mullvad ];
    })
  ]);
}
