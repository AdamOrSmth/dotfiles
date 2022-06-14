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
    # We make the rules ourselves instead of using the ones in nixpkgs
    # for full customization
    # See https://raw.githubusercontent.com/trustcrypto/trustcrypto.github.io/pages/49-onlykey.rules
    services.udev = {
      # Required for shebang and whatnot to work
      path = [ bash ];
      extraRules = ''
        ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", ENV{ID_MM_DEVICE_IGNORE}="1"
        ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", ENV{MTP_NO_PROBE}="1"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", OWNER:="${username}"

        ACTION=="add", SUBSYSTEMS=="usb", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", MODE:="0660", OWNER:="${username}", RUN+="${binDir}/ok-init"
        ACTION=="add", KERNEL=="ttyACM*", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", MODE:="0660", OWNER:="${username}", RUN+="${binDir}/ok-init"
      '';
    };

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
