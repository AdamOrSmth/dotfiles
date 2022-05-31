path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    setAttrByPath mkEnableOption getAttrFromPath mkIf optionalString;
  dataDir = "/var/lib/lokinet";
  inherit (pkgs) curl procps mullvad lokinet;
in {
  options =
    setAttrByPath path { enable = mkEnableOption "Lokinet router service"; };

  config = mkIf (getAttrFromPath path config).enable {
    users = {
      users.lokinet = {
        group = "lokinet";
        home = dataDir;
        createHome = true;
        isSystemUser = true;
      };
      groups.lokinet = {

      };
    };

    systemd.services.lokinet = {
      description = "Lokinet onion router";
      after = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ curl ];

      preStart = ''
        if [[ ! -f ${dataDir}/bootstrap.signed ]]; then
          ${curl}/bin/curl https://seed.lokinet.org/lokinet.signed -o /var/lib/lokinet/bootstrap.signed
        fi
      '';

      # `mullvad-exclude` requires root privileges on Nix (why?)
      postStart = optionalString config.my.hardware.networking.mullvad ''
        ${mullvad}/bin/mullvad split-tunnel pid add $(${procps}/bin/pgrep -x lokinet)
      '';

      serviceConfig = {
        User = "lokinet";
        Group = "lokinet";
        WorkingDirectory = dataDir;
        ExecStart = "${lokinet}/bin/lokinet /var/lib/lokinet/lokinet.ini";
        CapabilityBoundingSet = "CAP_NET_ADMIN CAP_NET_BIND_SERVICE";
        AmbientCapabilities = "CAP_NET_ADMIN CAP_NET_BIND_SERVICE";
        Restart = "always";
        RestartSec = "5s";
      };
    };

    networking.nameservers = [ "127.3.2.1" ];
  };
}
