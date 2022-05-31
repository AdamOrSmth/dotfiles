path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types all mkIf
    genAttrs;
  inherit (builtins) elem attrNames;
  cfg = getAttrFromPath path config;
  folders = [ "pass" "music" "sync" ];
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

  config = mkIf cfg.enable (let
    user =
      if config.my.user.enable then config.my.user.username else "syncthing";
  in {
    services.syncthing = {
      enable = true;
      openDefaultPorts = true;
      inherit (cfg) dataDir;
      configDir = config.users.users.${user}.home + "/.config/syncthing";
      devices = {
        pc = {
          id =
            "FEDF474-6KULWXQ-5H45SBU-YRSHPRZ-Z4UW3ZL-FR7R6P5-ZFSELXB-JPZB4A2";
        };
        laptop = {
          id =
            "NRUJM32-YLZB7LT-OWFMVB4-CCOHL7X-DNMIMUN-XZMUAX2-QXKSBSH-K4PSTA6";
        };
        hetzner = {
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
      in genAttrs (n: {
        inherit devices;
        path = mkPath n;
        enable = elem n cfg.enabledFolders;
      }) folders;
    };
  });
}
