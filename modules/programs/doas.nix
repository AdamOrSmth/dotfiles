{ config, lib, pkgs, ... }:

{
  security.sudo.enable = false;
  security.doas = {
    enable = true;
    extraRules = [{
      users = if config.ad.isServer then [ "root" ] else [ "ad" ];
      keepEnv = true;
      persist = true;
    }];
  };
}
