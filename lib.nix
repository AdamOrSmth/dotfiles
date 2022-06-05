# My custom utility functions (stolen from hlissner: https://github.com/hlissner/dotfiles/tree/master/lib)
{ lib, pkgs, inputs, system }:
let
  inherit (lib)
    filterAttrs hasSuffix hasPrefix mapAttrs' nameValuePair removeSuffix
    mapAttrsToList id concatLists genAttrs collect isFunction flatten;
  inherit (builtins) readDir attrValues;

  # Run a function on every `.nix` file in a directory, non-recursively,
  # and return the resulting attribute set in the format
  # filename-without-prefix = result-of-map-function.
  mapModules = dir: fn:
    let
      files = readDir dir;
      nixFiles =
        filterAttrs (n: _: hasSuffix ".nix" n && !(hasPrefix "_" n)) files;
    in mapAttrs'
    (n: v: nameValuePair (removeSuffix ".nix" n) (fn "${dir}/${n}")) nixFiles;

  # Like `mapModules`, but return a list consisting of only
  # the results of the map function.
  mapModules' = dir: fn: attrValues (mapModules dir fn);

  # Like `mapModules`, but do so recursively, descending into sub-directories as needed.
  mapModulesRec = dir: fn:
    let files = filterAttrs (n: _: !(hasPrefix "_" n)) (readDir dir);
    in mapAttrs' (n: v:
      let path = "${dir}/${n}";
      in if v == "directory" then
        nameValuePair n (mapModulesRec path fn)
      else
        nameValuePair (removeSuffix ".nix" n) (fn path)) files;

  # Like `mapModulesRec`, but return a list consisting of only
  # the results of the map function.
  mapModulesRec' = dir: fn:
    let
      dirs = mapAttrsToList (k: _: "${dir}/${k}")
        (filterAttrs (n: v: v == "directory" && !(hasPrefix "_" n))
          (readDir dir));
      files = mapModules' dir id;
      paths = files ++ concatLists (map (d: mapModulesRec' d id) dirs);
    in map fn paths;

  # Create a NixOS system given a hostname, with all common configuration settings and
  # whatnot.
  mkHost = host:
    lib.nixosSystem {
      inherit system;
      specialArgs = { inherit lib pkgs inputs system; };
      modules = flatten [
        { networking.hostName = host; }
        inputs.home-manager.nixosModules.home-manager
        inputs.self.nixosModules.common
        (collect isFunction inputs.self.nixosModules)
        (import ./hosts/${host}.nix)
      ];
    };
in { inherit mapModules mapModules' mapModulesRec mapModulesRec' mkHost; }
