{ config, lib, pkgs, ... }:

{
  config = lib.mkIf (!config.ad.isServer) {
    environment.systemPackages = with pkgs;
      [
        (gimp-with-plugins.override {
          plugins = with gimpPlugins; [ resynthesizer ];
        })
      ];
  };
}
