{ config, lib, pkgs, ... }:

{
  programs.fish.enable = true;
  # Programs used by my fish configuration
  environment.systemPackages = with pkgs; [ starship thefuck ];
}
