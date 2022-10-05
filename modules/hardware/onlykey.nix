path:

{ config, lib, pkgs, ... }:

let
  inherit (lib) setAttrByPath mkEnableOption getAttrFromPath mkIf;
  cfg = getAttrFromPath path config;
  # `libfido2` provides CLI commands
  inherit (pkgs) bash libfido2 gnupg;
  inherit (pkgs.my) onlykey-agent onlykey-bin onlykey-cli;
  inherit (config.my) binDir;
  inherit (config.my.user) username;
in {
  options = setAttrByPath path { enable = mkEnableOption "OnlyKey support"; };

  config = mkIf cfg.enable {
    hardware.onlykey.enable = true;

    environment = {
      systemPackages = [ onlykey-bin onlykey-cli onlykey-agent libfido2 gnupg ];
      sessionVariables.GNUPGHOME = "~/.gnupg/onlykey";
    };
  };
}
