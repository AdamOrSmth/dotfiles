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
      default = config.my.user.username;
    };
    key = mkOption {
      description = "Key to allow access with";
      type = types.nonEmptyStr;
    };
  };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      permitRootLogin = "no";
      passwordAuthentication = false;
      kbdInteractiveAuthentication = false;
      openFirewall = true;
      forwardX11 = true;
    };

    users.users.${cfg.user}.openssh.authorizedKeys.keys = [ cfg.key ];
  };
}
