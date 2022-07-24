path:

{ config, lib, pkgs, ... }:

let
  inherit (lib) setAttrByPath mkEnableOption getAttrFromPath mkIf;
  inherit (pkgs) buildGoModule fetchFromGitHub cgit;
  custom-caddy = let version = "2.5.2";
  in buildGoModule rec {
    pname = "caddy-custom";

    inherit version;

    src = fetchFromGitHub {
      owner = "AdamOrSmth";
      repo = "caddy";
      rev = "05e75016024098b2628cc572e9fc613e560de2a8";
      sha256 = "slMvBkTSm1E8o+CdXy1ZU8YMmrPXiVbZBhjb13w1Mrw=";
    };

    vendorSha256 = "cu8AcPAk18LcZwK+KGoqH6IjB4/BUg2ZnCQSB691ieQ=";
  };
in {
  options =
    setAttrByPath path { enable = mkEnableOption "Caddy reverse-proxying"; };

  config = mkIf (getAttrFromPath path config).enable {
    services.caddy = {
      enable = true;
      package = custom-caddy;
      group = "git";
      globalConfig = ''
        order cgi before file_server
      '';
      extraConfig = let cfg = config.services;
      in ''
        music.adamorsomething.xyz {
          reverse_proxy 127.0.0.1:${toString cfg.navidrome.settings.Port}
        }
        git.adamorsomething.xyz {
          handle_path /cgit* {
            root * ${cgit}/cgit
            file_server
          }
          handle {
            cgi * ${cgit}/cgit/cgit.cgi
          }
        }
      '';
    };

    systemd.services.caddy.serviceConfig.AmbientCapabilities =
      "CAP_NET_BIND_SERVICE";

    networking.firewall.allowedTCPPorts = [ 80 443 ];
  };
}
