{ lib, stdenv, fetchzip }:

stdenv.mkDerivation rec {
  pname = "nerd-fonts-symbols-only";
  version = "2.2.0-RC";

  src = fetchzip {
    url =
      "https://github.com/ryanoasis/nerd-fonts/releases/download/${version}/NerdFontsSymbolsOnly.zip";
    sha256 = "YT8WG0i2uoJLIBor6NlP2qNL2dGbAGRO2vlSLcdT0gM=";
    stripRoot = false;
  };

  installPhase = ''
    mkdir -p $out/share/fonts/truetype
    cp *.ttf $out/share/fonts/truetype
  '';
}
