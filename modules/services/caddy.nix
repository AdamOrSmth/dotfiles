path:

{ config, lib, pkgs, ... }:

let inherit (lib) setAttrByPath mkEnableOption getAttrFromPath mkIf;
in {
  options =
    setAttrByPath path { enable = mkEnableOption "Caddy reverse-proxying"; };

  config = mkIf (getAttrFromPath path config).enable {
    services.caddy = {
      enable = true;
      extraConfig = let cfg = config.services;
      in ''
        music.adamorsomething.xyz {
          reverse_proxy 127.0.0.1:${toString cfg.navidrome.settings.Port}
        }
        git.adamorsomething.xyz {
          reverse_proxy 127.0.0.1:${toString cfg.gitea.httpPort}
        }
      '';
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];
  };
}
