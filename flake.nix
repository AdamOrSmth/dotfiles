{
  description = "My NixOS configuration";

  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable"; };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";

      # Bootstrap custom library functions. Why is documentation for `extend` so hard to find?
      # Actually, that describes Nix in a nutshell. Y'know it's bad when the best example usage
      # I found of this method was in hlissner's dotfiles, which I basically stole the entire
      # line from.
      lib = nixpkgs.lib.extend (self: super: { my = import ./lib.nix self; });
      inherit (lib) id genAttrs singleton;
      inherit (lib.my) mapModules mapModulesRec mapModulesRec';
      inherit (builtins) attrNames split;

      pkgs = import nixpkgs {
        inherit system;
        # Add custom library functions here, there's probably a better way and place
        # to do this, but I'm not about to turn my dotfiles into an
        # over-engineered setup like hlissner, even though I kinda already
        # have. Just keep telling yourself that Ad, and eventually you'll have a config
        # that's just as monkey-patched and hacky as before! Who needs clean code, as long
        # as I understand the mess, it's fine right? Humans clearly never forget anything!
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
            # Hostname should always be the same, so it's only bootstrapped once during
            # initial setup.
            ({ ... }: { networking.hostName = host; })
          ];
        in lib.nixosSystem {
          inherit system;
          specialArgs = { inherit lib pkgs; };
          modules = common ++ modules ++ singleton (import ./hosts/${host}.nix);
        });
    };
}
