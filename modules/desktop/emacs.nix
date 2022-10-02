path:

{ config, lib, pkgs, inputs, system, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
  inherit (config.my) configDir;
in {
  options =
    setAttrByPath path { enable = mkEnableOption "Doom Emacs as editor"; };

  config = mkIf cfg.enable (mkMerge [{
    # Add binary cache for emacs-overlay
    nix.settings = {
      substituters = [ "https://nix-community.cachix.org" ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };

    # Enable Emacs daemon
    services.emacs = {
      enable = true;
      defaultEditor = true;
      package =
        # Install Emacs PGTK from the bleeding-edge overlay with native
        # compilation support and the `alpha-background` patch, and include
        # the `Vterm` package as it doesn't compile on NixOS
        ((pkgs.emacsPackagesFor
          (inputs.emacs-overlay.packages.${system}.emacsPgtkNativeComp)).emacsWithPackages
          (epkgs: [ epkgs.vterm ])); # :term vterm
    };

    # TODO Separate some of these into a separate module (mainly LaTeX)
    environment.systemPackages = with pkgs; [
      # Dependencies for base Doom
      git
      ripgrep
      fd
      coreutils
      # :checkers syntax
      proselint
      # :checkers spell
      (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
      # :checkers grammar
      languagetool
      # :lang org
      gcc # EmacSQL binary for `org-roam`
      gnuplot
      graphviz # Generating `org-roam` graphs
      pandoc
      # Exporting to LaTeX (https://nixos.wiki/wiki/TexLive)
      (texlive.combine {
        inherit (texlive)
          scheme-medium latexmk wrapfig ulem capt-of
          dvipng # Required by default by org-mode
          biber biblatex biblatex-mla biblatex-apa # Citations
          draftwatermark endfloat scalerel threeparttable apa7;
      })
      # :lang shell
      shellcheck
      nodePackages.bash-language-server
      # :email mu4e
      mu
      isync
      # GitHub Copilot
      nodejs-16_x
    ];

    fonts.fonts = builtins.attrValues {
      inherit (pkgs) comic-neue emacs-all-the-icons-fonts liberation_ttf;
    };

    my = {
      home = {
        configFiles."doom".source = "${configDir}/doom";
        # TODO Okay, this seriously needs to get split into separate modules
        files.".mbsyncrc".source = "${configDir}/doom/.mbsyncrc.crypt";
      };
      cli.fish.path = [ "${config.my.user.home}/.config/emacs/bin" ];
    };
  }]);
}
