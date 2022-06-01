{ config, lib, pkgs, ... }:

{
  config = lib.mkIf (!config.ad.isServer) { programs.firejail.enable = true; };
}
