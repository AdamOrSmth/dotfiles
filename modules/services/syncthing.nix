path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types all mkIf
    genAttrs;
  inherit (builtins) elem attrNames;
  cfg = getAttrFromPath path config;
  folders = [ "Pass" "Music" "Sync" ];
in {
  options = setAttrByPath path {
    enable = mkEnableOption "Syncthing service";
    dataDir = mkOption {
      description = "Default data location";
      type = types.path;
      default = "/var/lib/syncthing";
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
  };

  config = mkIf cfg.enable (let user = config.my.user;
  in {
    services.syncthing = {
      enable = true;
      user = user.username;
      openDefaultPorts = true;
      inherit (cfg) dataDir;
      configDir = "${user.home}/.config/syncthing";
      devices = {
        AdPC = {
          id =
            "ZCUJPS3-DS2HJXR-WXNDHY2-7TF6CCW-SUN2EIF-E7GCFYT-IGUY4CN-GY4HMQO";
        };
        AdLaptop = {
          id =
            "AENWMMW-DELUEVT-QXCO45X-TO55E7B-3JU25OD-ACDVOIN-FTQT7CJ-CKBCEQH";
        };
        AdServer = {
          id =
            "VYNPC54-JKPBHIF-4NI4TEX-V33QRJ6-QXBHIS5-ZXYDUJY-3E33GM7-X2UFYAG";
        };
      };
      folders = let
        mkPath = (folder:
          if cfg.folderLocations ? folder then
            cfg.folderLocations.${folder}
          else
            "${cfg.dataDir}/${folder}");
        # Generate a list of devices from the devices attribute
        devices =
          (lib.mapAttrsToList (n: _: n) config.services.syncthing.devices);
      in genAttrs folders (n: {
        inherit devices;
        path = mkPath n;
        enable = elem n cfg.enabledFolders;
      });
    };
  });
}
