path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types all mkIf
    genAttrs;
  inherit (builtins) elem attrNames;
  cfg = getAttrFromPath path config;
  folders = [ "Pass" "Music" "Sync" ];
  user = if cfg.system then "syncthing" else config.my.user.username;
  home = if cfg.system then "/var/lib/syncthing" else config.my.user.home;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "Syncthing service";
    system = mkEnableOption "Run as system user + service";
    dataDir = mkOption {
      description = "Default data location";
      type = types.path;
      default = home;
    };
    enabledFolders = mkOption {
      description = "List of folders to enable on this device";
      type = types.addCheck (types.listOf types.nonEmptyStr)
        (l: all (f: elem f folders) l);
      default = folders;
    };
    folderLocations = mkOption {
      description = "Custom folder locations (folder name -> location path)";
      type = types.addCheck (types.attrsOf types.path)
        (l: all (f: elem f folders) (attrNames l));
      default = { };
    };
    receiveEncrypted = mkOption {
      description = "List of folders to set to `receiveencrypted`";
      type = types.addCheck (types.listOf types.nonEmptyStr)
        (l: all (f: elem f folders) l);
      default = [ ];
    };
  };

  config = mkIf cfg.enable ({
    services.syncthing = {
      enable = true;
      inherit user;
      openDefaultPorts = true;
      inherit (cfg) dataDir;
      configDir = "${home}/.config/syncthing";
      devices = {
        AdPC.id =
          "ZCUJPS3-DS2HJXR-WXNDHY2-7TF6CCW-SUN2EIF-E7GCFYT-IGUY4CN-GY4HMQO";
        AdLaptop.id =
          "AENWMMW-DELUEVT-QXCO45X-TO55E7B-3JU25OD-ACDVOIN-FTQT7CJ-CKBCEQH";
        AdServer.id =
          "XPDW2XO-HPQPHCH-5GXJOW2-BQH7R6Z-FDIFJ5X-MSFOOZR-PC3W2N6-XLV5BQ7";
      };
      folders = let
        # Generate a list of devices from the devices attribute
        devices =
          (lib.mapAttrsToList (n: _: n) config.services.syncthing.devices);
      in genAttrs folders (n: {
        inherit devices;
        path = cfg.folderLocations.${n} or "${cfg.dataDir}/${n}";
        enable = elem n cfg.enabledFolders;
        type = if elem n cfg.receiveEncrypted then
          "receiveencrypted"
        else
          "sendreceive";
      });
    };
  });
}
