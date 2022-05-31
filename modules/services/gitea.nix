path:

{ config, lib, pkgs, ... }:

let inherit (lib) setAttrByPath mkEnableOption getAttrFromPath mkIf;
in {
  options = setAttrByPath path { enable = mkEnableOption "Gitea server"; };

  config = mkIf (getAttrFromPath path config).enable {
    services.gitea = let domain = "git.adamorsomething.xyz";
    in {
      enable = true;
      appName = "Ad's Gitea";
      inherit domain;
      rootUrl = "https://${domain}";
      cookieSecure = true;
    };
  };
}
