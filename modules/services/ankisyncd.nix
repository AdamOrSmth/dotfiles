path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  inherit (config.my) configDir;
  cfg = getAttrFromPath path config;
  package = pkgs.my.ankisyncd-rs;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "self-hosted Anki sync server";
  };

  config = mkIf cfg.enable (mkMerge [{
    systemd.services.ankisyncd = {
      description = "ankisyncd - Anki sync server";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ package ];

      serviceConfig = {
        Type = "simple";
        DynamicUser = true;
        StateDirectory = "ankisyncd";
        ExecStart = "${package}/bin/ankisyncd -c ${
            pkgs.writeText "ankisyncd.toml" ''
              [listen]
              host = "127.0.0.1"
              port = 27701

              [paths]
              root_dir = "/var/lib/ankisyncd"

              [encryption]
              ssl_enable = false
              cert_file = ""
              key_file = ""
            ''
          }";
        Restart = "always";
      };
    };
    my.services.caddy.config = ''
      anki.adamorsomething.xyz {
        reverse_proxy 127.0.0.1:27701
      }
    '';
    # Needed to create a user
    environment.systemPackages = [ pkgs.my.ankisyncd-rs ];
  }]);
}
