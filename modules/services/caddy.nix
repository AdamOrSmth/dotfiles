path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    setAttrByPath mkEnableOption getAttrFromPath mkIf mkOption types;
  inherit (pkgs) buildGoModule fetchgit cgit;
  custom-caddy = let version = "2.5.2";
  in buildGoModule rec {
    pname = "caddy-custom";

    inherit version;

    src = fetchgit {
      url = "https://git.adamorsomething.xyz/caddy.git";
      rev = "8272013fc9e64cb4dcb1b451a69089cf73fa9be6";
      sha256 = "";
    };

    vendorSha256 = "cu8AcPAk18LcZwK+KGoqH6IjB4/BUg2ZnCQSB691ieQ=";
  };
  cfg = getAttrFromPath path config;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "Caddy reverse-proxying";
    config = mkOption {
      description = "Lines to add to Caddyfile";
      type = types.lines;
    };
  };

  config = mkIf cfg.enable {
    services.caddy = {
      enable = true;
      package = custom-caddy;
      group = "git";
      globalConfig = ''
        order cgi before file_server
      '';
      extraConfig = cfg.config;
    };

    # Allows Caddy to bind to small port numbers
    systemd.services.caddy.serviceConfig.AmbientCapabilities =
      "CAP_NET_BIND_SERVICE";

    networking.firewall.allowedTCPPorts = [ 80 443 ];
  };
}
