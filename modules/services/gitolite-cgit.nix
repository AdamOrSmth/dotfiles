path:

{ config, lib, pkgs, ... }:

let inherit (lib) setAttrByPath mkEnableOption getAttrFromPath mkIf;
in {
  options = setAttrByPath path {
    enable = mkEnableOption "Gitolite + cgit self-hosted Git server";
  };

  config = mkIf (getAttrFromPath path config).enable {
    services.gitolite = { enable = true; };
  };
}
