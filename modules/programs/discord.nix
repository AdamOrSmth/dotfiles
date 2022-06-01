{ config, lib, pkgs, ... }:

{
  config = lib.mkIf (!config.ad.isServer) {
    # Proprietary
    nixpkgs.config.allowUnfreePredicate = pkg:
      builtins.elem (lib.getName pkg) [ "discord" ];
    programs.firejail.wrappedBinaries = {
      discord = {
        executable = "${pkgs.discord}/bin/discord";
        profile = "${pkgs.firejail}/etc/firejail/discord.profile";
      };
    };
  };
}
