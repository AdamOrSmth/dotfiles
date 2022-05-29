{
  description = "My NixOS configuration";

  inputs = { nixpkgs = { url = "github:NixOS/nixpkgs/nixos-unstable"; }; };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
      commonConfig =
        [ ({ ... }: { nixpkgs.overlays = [ import ./packages ]; }) ];
      hosts = [ "AdPC" "AdLaptop" ];
      modules = [ ];
      inherit (nixpkgs) lib;
    in {
      nixosConfigurations = lib.attrsets.genAttrs hosts (host:
        lib.nixosSystem {
          inherit system;
          modules = commonConfig ++ modules ++ [ import ./hosts/${host}.nix ];
        });
    };
}
