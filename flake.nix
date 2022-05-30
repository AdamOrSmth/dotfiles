{
  description = "My NixOS configuration";

  inputs = { nixpkgs = { url = "github:NixOS/nixpkgs/nixos-unstable"; }; };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      # Bootstrap custom library functions here, since the overlay
      # isn't loaded until later
      lib = nixpkgs.lib.extend (self: super: { my = import ./lib.nix self; });
      inherit (lib.my) mapModules mapModulesRec mapModulesRec';
      system = "x86_64-linux";
      hosts = builtins.attrNames (mapModules ./hosts lib.id);
      # Import each module, passing it its relative path,
      # which is used to define options. Engineering completely over-complicated solutions
      # for not-very-problematic problems is great, isn't it? Automation go brr.
      # There's probably a better way to do this, but I'm working on this way too late at night,
      # so I really don't feel like finding it.
      modules = mapModulesRec' ./modules (p:
        let inherit (lib) drop flatten removeSuffix;
        in import p ([ "my" ]
          ++ (drop 4 (flatten (builtins.split "/" (removeSuffix ".nix" p))))));
    in {
      nixosConfigurations = lib.genAttrs hosts (host:
        let
          common = [
            (import ./overlay.nix)
            # Hostname should always be the same
            ({ ... }: { networking.hostName = host; })
          ];
        in lib.nixosSystem {
          inherit system;
          modules = common ++ modules
            ++ lib.singleton (import ./hosts/${host}.nix);
        });
    };
}
