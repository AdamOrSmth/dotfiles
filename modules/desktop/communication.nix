path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
  inherit (pkgs) discord firejail;
  configDir = "${config.my.configDir}/betterdiscord";
in {
  options = setAttrByPath path {
    enable = mkEnableOption "chat and communication programs";
  };

  config = mkIf cfg.enable (mkMerge [{
    # Wrap Discord, since it's proprietary
    # TODO Create a separate Firejail module
    programs.firejail = {
      enable = true;
      wrappedBinaries = {
        discord = {
          executable = "${discord}/bin/discord";
          profile = "${firejail}/etc/firejail/discord.profile";
        };
      };
    };
    environment.systemPackages = builtins.attrValues {
      inherit (pkgs) element-desktop session-desktop-appimage signal-desktop;
    };

    # TODO Separate Discord into a separate module
    my.home.configFiles = {
      "BetterDiscord/themes".source = "${configDir}/themes";
      "BetterDiscord/plugins" = {
        source = "${configDir}/plugins";
        recursive = true; # Need write access to the plugins directory
      };
    };
  }]);
}
