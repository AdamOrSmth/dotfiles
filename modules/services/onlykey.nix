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
  options = setAttrByPath path {
    enable = mkEnableOption "OnlyKey packages and agent service";
  };

  config = mkIf cfg.enable {
    hardware.onlykey.enable = true;

    environment = {
      systemPackages = [ onlykey-bin onlykey-cli onlykey-agent libfido2 gnupg ];
      variables.GNUPGHOME = "~/.gnupg/onlykey";
    };

    # See https://github.com/romanz/trezor-agent/blob/master/doc/README-GPG.md
    systemd.user = {
      services.onlykey-gpg-agent = {
        description = "onlykey-gpg-agent";
        requires = [ "onlykey-gpg-agent.socket" ];

        path = [ gnupg ];

        serviceConfig = {
          Type = "simple";
          Environment = [ "GNUPGHOME=%h/.gnupg/onlykey" ];
          ExecStart = "${onlykey-agent}/bin/onlykey-gpg-agent";
        };

        wantedBy = [ "onlykey-gpg-agent.socket" ];
      };
      sockets.onlykey-gpg-agent = {
        description = "onlykey-gpg-agent socket";
        listenStreams = [ "%t/gnupg/S.gpg-agent" ];
        socketConfig = {
          FileDescriptorName = "std";
          SocketMode = 600;
          DirectoryMode = 700;
        };
        wantedBy = [ "sockets.target" ];
      };
    };
  };
}
