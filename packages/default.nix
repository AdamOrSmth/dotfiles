(self: super:
  let callPkg = super.callPackage;
  in {
    iosekva-aile = callPkg ./iosevka-aile.nix { };
    iosekva-etoile = callPkg ./iosevka-etoile.nix { };
    lokinet = callPkg ./lokinet.nix { };
    nerd-fonts-symbols-only = callPkg ./nerd-fonts-symbols-only.nix { };
    onlykey-bin = callPkg ./onlykey-bin.nix { };
    onlykey-agent =
      callPkg ./onlykey-agent.nix { inherit (super) onlykey-agent; };
    onlykey-cli = callPkg ./onlykey-cli.nix { inherit (super) onlykey-cli; };
    shell-color-scripts = callPkg ./shell-color-scripts.nix { };
  })
