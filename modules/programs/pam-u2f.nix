{ config, lib, pkgs, ... }:

{
  config = lib.mkIf (!config.ad.isServer) {
    security.pam.u2f = {
      enable = true;
      cue = true;
    };
  };
}
