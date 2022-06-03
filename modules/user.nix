path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    username = mkOption {
      description = "Username of personal user";
      type = types.nonEmptyStr;
      default = "ad";
    };
    extraGroups = mkOption {
      description = "Extra groups for personal user";
      type = types.listOf types.nonEmptyStr;
      default = [ "wheel" ];
    };
    home = mkOption {
      description = "Home directory of personal user";
      type = types.path;
      default = "/home/${cfg.username}";
    };
  };

  config = mkMerge [{
    users.users.${cfg.username} = {
      isNormalUser = true;
      extraGroups = cfg.extraGroups;
      home = cfg.home;
      createHome = true;
      # Make sure to change password on fresh install!
      initialPassword = "ad";
    };
  }];
}
