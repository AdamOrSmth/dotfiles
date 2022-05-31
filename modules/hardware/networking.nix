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

  config = mkIf cfg.enable (mkMerge [
    {
      networking.useDHCP = false; # Deprecated flag
      networking.interfaces.${cfg.interface}.useDHCP = true;
      networking.wireless.enable = cfg.wireless;
    }
    (mkIf cfg.mullvad {
      services.mullvad-vpn.enable = !config.ad.isServer;
      environment.systemPackages = [ pkgs.mullvad ];
    })
  ]);
}
