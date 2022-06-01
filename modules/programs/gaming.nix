{ config, lib, pkgs, ... }:

{
  config = lib.mkIf (!config.ad.isServer) {
    nixpkgs.config.allowUnfree = true;
    #nixpkgs.config.allowUnfreePredicate = pkg:
    #  builtins.elem (lib.getName pkg) [
    #    "steam"
    #    "steam-original"
    #    "steam-runtime"
    #  ];
    programs = {
      steam.enable = true;
      gamemode.enable = true;
    };
    environment.systemPackages = with pkgs; [ lutris polymc ];
  };
}
