path:

{ config, lib, pkgs, ... }:

let
  inherit (lib) getAttrFromPath setAttrByPath mkEnableOption mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path { enable = mkEnableOption "printing services"; };

  config = mkIf cfg.enable (mkMerge [{
    services.printing = {
      enable = true;
      drivers = with pkgs; [ hplip ];
    };
    services.avahi.enable = true;
    services.avahi.nssmdns = true;

    # Enable scanning
    hardware.sane = {
      enable = true;
      extraBackends = with pkgs; [ sane-airscan ];
    };

    my.user.extraGroups = [ "scanner" "lp" ];
  }]);
}
