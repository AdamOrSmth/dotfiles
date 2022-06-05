# My custom utility functions (stolen from hlissner: https://github.com/hlissner/dotfiles/tree/master/lib)
{ lib, pkgs, inputs, system }:
let
  inherit (lib)
    filterAttrs hasSuffix hasPrefix mapAttrs' nameValuePair removeSuffix
    mapAttrsToList id concatLists genAttrs collect isFunction;
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
    let
      common = [
        (import ./hosts/_common.nix { inherit host lib; })
        # `home-manager` provides a module to use with a full NixOS configuration
        # that we need to import to use. I can't think of a way to add it conditionally
        # depending on whether the configuration is enabled, and I can't be bothered to
        # think harder, and the input exists anyway, so I'm just gonna import it unconditionally.
        inputs.home-manager.nixosModules.home-manager
      ];
    in lib.nixosSystem {
      inherit system;
      specialArgs = { inherit lib pkgs inputs system; };
      modules = common ++ (collect isFunction inputs.self.nixosModules)
        ++ lib.singleton (import ./hosts/${host}.nix);
    };
in { inherit mapModules mapModules' mapModulesRec mapModulesRec' mkHost; }
