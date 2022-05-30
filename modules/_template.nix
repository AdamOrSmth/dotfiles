path:

{ config, lib, pkgs, ... }:

let inherit (lib) setAttrByPath mkEnableOption getAttrFromPath mkIf;
in {
  options = setAttrByPath path { enable = throw "Create some options here!"; };

  config = mkIf (getAttrFromPath path config).enable {
    setting = throw "Set some options here!";
  };
}
