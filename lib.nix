# My custom utility functions (stolen from hlissner: https://github.com/hlissner/dotfiles/tree/master/lib)
lib:
let
  inherit (lib)
    filterAttrs hasSuffix hasPrefix mapAttrs' nameValuePair removeSuffix
    mapAttrsToList id concatLists;
  inherit (builtins) readDir attrValues;

  # Run a function on every `.nix` file in a directory, non-recursively,
  # and return the resulting attribute set in the format
  # filename-without-prefix = result-of-map-function.
  # mapModules ::
  #   path -> (path -> any) -> attrs
  mapModules = dir: fn:
    let
      files = readDir dir;
      nixFiles =
        filterAttrs (n: _: hasSuffix ".nix" n && !(hasPrefix "_" n)) files;
    in mapAttrs'
    (n: v: nameValuePair (removeSuffix ".nix" n) (fn "${dir}/${n}")) nixFiles;

  # Like `mapModules`, but return a list consisting of only
  # the results of the map function.
  # mapModules' ::
  #   path -> (path -> any) -> [any]
  mapModules' = dir: fn: attrValues (mapModules dir fn);

  # Like `mapModules`, but do so recursively, descending into sub-directories as needed.
  # mapModulesRec ::
  #   path -> (path -> any) -> attrs
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
  # mapModulesRec ::
  #   path -> (path -> any) -> [any]
  mapModulesRec' = dir: fn:
    let
      dirs = mapAttrsToList (k: _: "${dir}/${k}")
        (filterAttrs (n: v: v == "directory" && !(hasPrefix "_" n))
          (readDir dir));
      files = mapModules' dir id;
      paths = files ++ concatLists (map (d: mapModulesRec' d id) dirs);
    in map fn paths;
in { inherit mapModules mapModules' mapModulesRec mapModulesRec'; }
