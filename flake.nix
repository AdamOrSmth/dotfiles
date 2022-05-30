{
  description = "My NixOS configuration";

  inputs = { nixpkgs = { url = "github:NixOS/nixpkgs/nixos-unstable"; }; };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      inherit (nixpkgs) lib;
      system = "x86_64-linux";
      modules = [ ];
      hosts = lib.mapAttrsToList (n: _: lib.removeSuffix ".nix" n)
        (builtins.readDir ./hosts);
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
