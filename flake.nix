{
  description = "My NixOS configuration";

  inputs = { nixpkgs = { url = "github:NixOS/nixpkgs/nixos-unstable"; }; };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
      commonConfig = [ (import ./overlay.nix) ];
      hosts = [ "AdPC" "AdLaptop" ];
      modules = [ ];
      inherit (nixpkgs) lib;
    in {
      nixosConfigurations = lib.attrsets.genAttrs hosts (host:
        lib.nixosSystem {
          inherit system;
          modules = commonConfig ++ modules
            ++ lib.singleton (import ./hosts/${host}.nix);
        });
    };
}
