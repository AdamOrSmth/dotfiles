path:

{ config, lib, pkgs, inputs, system, ... }:

let
  inherit (lib)
    getAttrFromPath setAttrByPath mkEnableOption mkOption types mkIf mkMerge;
  cfg = getAttrFromPath path config;
  inherit (config.my) home configDir;
in {
  options =
    setAttrByPath path { enable = mkEnableOption "Doom Emacs as editor"; };

  config = mkIf cfg.enable (mkMerge [{
    # Fonts used in my config
    fonts.fonts = with pkgs; [ nerd-fonts-symbols-only comic-neue ];

    # Enable Emacs daemon
    services.emacs = {
      enable = true;
      defaultEditor = true;
      package =
        # Install Emacs 28 from the bleeding-edge overlay with native
        # compilation support and the `alpha-background` patch, and include
        # the `Vterm` package as it doesn't compile on NixOS
        ((pkgs.emacsPackagesFor
          (inputs.emacs-overlay.packages.${system}.emacsNativeComp.overrideAttrs
            (oldAttrs: {
              patches = [
                (pkgs.fetchurl {
                  url =
                    "https://raw.githubusercontent.com/TheVaffel/emacs/master/emacs_background_transparency.patch";
                  sha256 = "yt4NC1CYbq3FhIRnoUZ/YHmEu5KoHVAdLssB/nStVfk=";
                })
              ];
            }))).emacsWithPackages (epkgs: [ epkgs.vterm ])); # :term vterm
    };

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
      pandoc # Exporting
      # Exporting to LaTeX (https://nixos.wiki/wiki/TexLive)
      (texlive.combine {
        inherit (texlive)
          scheme-medium # Medium because there are some font packages that I don't feel like tracking down
          dvisvgm dvipng wrapfig amsmath ulem hyperref capt-of spverbatim
          titlesec mla-paper biblatex-mla;
      })
      # :lang shell
      shellcheck
      # :app everywhere
      xclip
      xdotool
      xorg.xwininfo
    ];

    home.configFiles."doom".source = "${configDir}/doom";
  }]);
}
