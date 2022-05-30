# List of all overlays to import in `flake.nix`
[
  # Custom derivations
  # Generate a list of files in `packages` sub-directory, filter to only .nix files,
  # and then add each one to the overlay
  (self: super: super.lib.my.mapModules ./packages (p: super.callPackage p { }))
]
