path:

{ config, lib, pkgs, ... }:

let inherit (lib) setAttrByPath mkEnableOption getAttrFromPath mkIf;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "Gitolite + cgit self-hosted Git server";
  };

  config = mkIf (getAttrFromPath path config).enable {
    services.gitolite = {
      enable = true;
      user = "git";
      group = "git";
      adminPubkey =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEBf93IkfW57z/aCDd+elE5nQ7LamVVDVAFr3n4oWPzj <ssh://git@git.adamorsomething.xyz|ed25519>";
    };

    environment.etc.cgitrc.text = ''
      scan-path=/var/lib/gitolite/repositories

      root-title=Ad's Repos
    '';
  };
}
