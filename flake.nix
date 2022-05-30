{
  description = "My NixOS configuration";

  inputs = { nixpkgs = { url = "github:NixOS/nixpkgs/nixos-unstable"; }; };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      # Bootstrap custom library functions here, since the overlay
      # isn't loaded until later
      lib = nixpkgs.lib.extend (self: super: { my = import ./lib.nix self; });
      inherit (lib.my) mapModules mapModulesRec';
      system = "x86_64-linux";
      hosts = builtins.attrNames (mapModules ./hosts lib.id);
      modules = mapModulesRec' ./modules lib.id;
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
