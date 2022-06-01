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
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";

      # Bootstrap custom library functions. Why is documentation for `extend` so hard to find?
      # Actually, that describes Nix in a nutshell. Y'know it's bad when the best example usage
      # I found of this method was in hlissner's dotfiles, which I basically stole the entire
      # line from.
      lib = nixpkgs.lib.extend (self: super: { my = import ./lib.nix self; });
      inherit (lib) getName id genAttrs singleton;
      inherit (lib.my) mapModules mapModulesRec mapModulesRec';
      inherit (builtins) elem attrNames split;

      pkgs = import nixpkgs {
        inherit system;
        # Forgive me, oh merciful lords of FOSS, for I have committed a sin.
        # Curse you, Nvidia, for leaving me no other option. Only time will
        # tell if your open source drivers become anything close to good, and if
        # so, I shall forgive you.
        # Oh yeah also I use Steam and Discord. I think that's it though. They,
        # too, can go on the unholy list of shame.
        config.allowUnfreePredicate = pkg:
          elem (getName pkg) [
            "nvidia-x11"
            "nvidia-settings"
            "steam"
            "discord"
          ];

        # Add custom library functions into top-level `pkgs` here, there's
        # probably a better way and place to do this, but I'm not about to
        # turn my dotfiles into an over-engineered setup like hlissner, even
        # though I kinda already have. Just keep telling yourself that Ad,
        # and eventually you'll have a config that's just as monkey-patched
        # and hacky as before! Who needs clean code, as long as I understand
        # the mess, it's fine right? Humans clearly never forget anything!
        overlays = singleton (self: super: { inherit lib; })
          ++ import ./overlay.nix;
      };

      hosts = attrNames (mapModules ./hosts id);

      # Import each module, passing it its relative path,
      # which is used to define options. Engineering completely over-complicated solutions
      # for not-very-problematic problems is great, isn't it? Automation go brr.
      # There's probably a better way to do this, but I'm working on this way too late at night,
      # so I really don't feel like finding it.
      modules = mapModulesRec' ./modules (p:
        let inherit (lib) pipe drop flatten removeSuffix concat;
        in import p (pipe p [
          (removeSuffix ".nix")
          (split "/")
          flatten
          (drop 4)
          (concat [ "my" ])
        ]));

    in {
      nixosConfigurations = genAttrs hosts (host:
        let
          common = [
            (import ./hosts/_common.nix host)
            # `home-manager` provides a module to use with a full NixOS configuration
            # that we need to import to use. I can't think of a way to add it conditionally
            # depending on whether the configuration is enabled, and I can't be bothered to
            # think harder, and the input exists anyway, so I'm just gonna import it unconditionally.
            home-manager.nixosModules.home-manager
          ];
        in lib.nixosSystem {
          inherit system;
          specialArgs = { inherit lib pkgs; };
          modules = common ++ modules ++ singleton (import ./hosts/${host}.nix);
        });
    };
}
