# Programs with extra settngs/configurations
{ config, lib, pkgs, ... }:

{
  imports = [
    ./bspwm.nix
    ./discord.nix
    ./doas.nix
    ./doom-emacs.nix
    ./firejail.nix
    ./fish.nix
    ./gaming.nix
    ./gimp.nix
    ./megasync.nix
    ./nix.nix
    ./pam-u2f.nix
    ./pass.nix
  ];
}
