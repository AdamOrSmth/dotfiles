{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.ad.isPC {
    nixpkgs.config.allowUnfreePredicate = pkg:
      builtins.elem (lib.getName pkg)
      [ "megasync" ]; # Open source but not libre
    environment.systemPackages = with pkgs; [ megasync ];
  };
}
