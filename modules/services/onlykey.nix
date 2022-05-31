path:

{ config, lib, pkgs, ... }:

let
  inherit (lib) setAttrByPath mkEnableOption getAttrFromPath mkIf;
  cfg = getAttrFromPath path config;
  # `libfido2` provides CLI commands
  inherit (pkgs) libfido2 gnupg onlykey-agent onlykey-bin onlykey-cli;
  gpgEnabled = config.my.cli.gnupg.enable;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "OnlyKey packages and agent service";
  };

  config = mkIf cfg.enable {
    hardware.onlykey.enable = true;

    environment = {
      systemPackages = [ onlykey-bin onlykey-cli onlykey-agent libfido2 gnupg ];
    } // mkIf gpgEnabled { variables.GNUPGHOME = "~/.gnupg/onlykey"; };

    # See https://github.com/romanz/trezor-agent/blob/master/doc/README-GPG.md
    systemd.user = mkIf gpgEnabled {
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
