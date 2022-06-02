path:

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
in {
  options =
    setAttrByPath path { enable = mkEnableOption "direnv and nix-direnv"; };

  config = mkIf cfg.enable (mkMerge [{
    environment.systemPackages =
      builtins.attrValues { inherit (pkgs) direnv nix-direnv; };
    # Nix options to persist garbage collection
    nix.extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
    # For sourcing the setup file from $XDG_CONFIG_DIR/.direnvrc
    environment.pathsToLink = [ "/share/nix-direnv" ];

    my = {
      cli.fish.extraInit = "direnv hook fish | source";
      home.configFiles."direnv/direnvrc".text =
        "source /run/current-system/sw/share/nix-direnv/direnvrc";
    };
  }]);
}
