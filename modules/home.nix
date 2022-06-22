path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "home-manager configuration";
    files = mkOption {
      description = "Files to place in home directory";
      type = types.attrs;
      default = { };
    };
    configFiles = mkOption {
      description = "Files to place in config directory";
      type = types.attrs;
      default = { };
    };
    dataFiles = mkOption {
      description = "Files to place in data directory";
      type = types.attrs;
      default = { };
    };
  };

  config = mkIf cfg.enable (mkMerge [{
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      users.${config.my.user.username} = {
        home = {
          file = cfg.files;
          stateVersion = config.system.stateVersion;
        };
        xdg = {
          enable = true;
          configFile = cfg.configFiles;
          dataFile = cfg.dataFiles;
        };
      };
    };
  }]);
}
