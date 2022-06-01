{ config, lib, pkgs, ... }:

{
  config = lib.mkIf (!config.ad.isServer) {
    environment.systemPackages = with pkgs;
      [
        (pass.withExtensions
          (exts: with exts; [ pass-otp pass-audit pass-update ]))
      ];
  };
}
