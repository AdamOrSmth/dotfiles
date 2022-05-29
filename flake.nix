{
  description = "My NixOS configuration";

  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable"; };

  outputs = { nixpkgs, ... }@inputs:
    let system = "x86_64-linux";
    in {
      nixosConfigurations = {
        AdPC = nixpkgs.lib.nixosSystem { inherit system; };
        AdLaptop = nixpkgs.lib.nixosSystem { inherit system; };
      };
    };
}
