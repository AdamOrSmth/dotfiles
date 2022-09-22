# Ad/dotfiles --- flake.nix
#
# URL:     https://git.adamorsomething.xyz/Ad/dotfiles
# License: MIT
#
# Welcome to the main brains of the operation. Suit up, soldier. You
# have a long mission ahead of you.
#
# Yes, this file is way too long and bloated for what it should be doing,
# but I'm too lazy to properly split it up, so here we are. Enjoy your stay.

{
  description = "My NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland = {
      url = "github:vaxerski/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";

      # Bootstrap custom library functions.
      lib = nixpkgs.lib.extend (final: prev: {
        my = import ./lib.nix {
          inherit pkgs inputs system;
          lib = prev;
        };
      });
      inherit (lib.my) mapModules mapModulesRec;

      pkgs = import nixpkgs {
        inherit system;
        # Wall of shame.
        config.allowUnfreePredicate = pkg:
          builtins.elem (lib.getName pkg) [
            "nvidia-x11"
            "nvidia-settings"
            "steam"
            "steam-original"
            "discord"
          ];
        # Import this flake's overlays. Lazy evaluation is great!
        overlays = builtins.attrValues self.overlays;
      };

      hosts = builtins.attrNames (mapModules ./hosts lib.id);
    in {
      # Load custom package derivations.
      packages.${system} = mapModules ./packages (p: pkgs.callPackage p { });

      # Define an overlay for aforementioned custom package derivations.
      overlays.default = (final: prev: { my = self.packages.${system}; });

      # All modules for my configurations. Separation of concerns I suppose.
      # Import each module, passing it its relative path, which is used to
      # define options. Engineering completely over-complicated solutions
      # for not-very-problematic problems is great, isn't it?
      nixosModules = {
        common = import ./hosts/_common.nix { inherit lib inputs; };
      } // mapModulesRec ./modules (p:
        let
          inherit (lib) pipe removeSuffix flatten drop concat;
          inherit (builtins) split;
        in import p (pipe p [
          (removeSuffix ".nix")
          (split "/")
          flatten
          (drop 4)
          (concat [ "my" ])
        ]));

      nixosConfigurations = lib.genAttrs hosts lib.my.mkHost;
    };
}
