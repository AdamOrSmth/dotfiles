# Woo, proprietary Electron garbage!
{ lib, stdenv, fetchurl, dpkg, makeWrapper, steam-run }:

stdenv.mkDerivation {
  pname = "tetrio-desktop";
  version = "unknown";

  src = fetchurl {
    url = "https://tetr.io/about/desktop/builds/TETR.IO%20Setup.deb";
    sha256 = "rgcRD4hpLhSF9+8dOrXv+Vd0dGYxsXgz4ozAm6Gji9o=";
  };

  nativeBuildInputs = [ dpkg makeWrapper ];

  buildInputs = [ ];

  unpackCmd = "dpkg -x $curSrc unpacked";

  installPhase = ''
    mkdir -p $out/bin
    cp -r opt $out
    echo "${steam-run}/bin/steam-run $out/opt/TETR.IO/tetrio-desktop" > $out/bin/tetrio-desktop
    chmod +x $out/bin/tetrio-desktop
    cp -r usr/share $out
    substituteInPlace $out/share/applications/tetrio-desktop.desktop \
      --replace "/opt/TETR.IO/" "$out/bin/"
  '';
}
