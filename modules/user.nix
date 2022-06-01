path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    username = mkOption {
      description = "Username of personal account";
      type = types.nonEmptyStr;
      default = "ad";
    };
    extraGroups = mkOption {
      description = "Extra groups for personal account";
      type = types.listOf types.nonEmptyStr;
      default = [ ];
    };
  };

  config = mkMerge [{
    users.users.${cfg.username} = {
      isNormalUser = true;
      extraGroups = [ "wheel" ] ++ cfg.extraGroups;
      home = "/home/${cfg.username}";
    };
  }];
}
