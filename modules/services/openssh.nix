path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption mkIf types;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "OpenSSH access";
    user = mkOption {
      description = "User to allow SSH access as";
      type = types.nonEmptyStr;
      default =
        if config.my.user.enable then config.my.user.username else "root";
    };
    key = mkOption {
      description = "Key to allow access with";
      type = types.nonEmptyStr;
    };
  };

  config = mkIf cfg.enable {
    services.openssh.enable = true;

    users.users.${cfg.user}.openssh.authorizedKeys.keys = [ cfg.key ];

    networking.firewall.allowedTCPPorts = [ 22 ];
  };
}
