# Woo, proprietary Electron garbage!
{ lib, stdenv, fetchurl, dpkg, autoPatchelfHook, glib, libX11, libxcb
, libXcomposite, libXcursor, libXdamage, libXext, libXfixes, libXi, libXrender
, libXtst, dbus, gtk3, pango, libXrandr, libdrm, cups, nss, nspr, mesa
, libXScrnSaver, alsa-lib, cairo, atk, gdk-pixbuf, expat }:

stdenv.mkDerivation {
  pname = "tetrio-desktop";
  version = "unknown";

  src = fetchurl {
    url = "https://tetr.io/about/desktop/builds/TETR.IO%20Setup.deb";
    sha256 = "rgcRD4hpLhSF9+8dOrXv+Vd0dGYxsXgz4ozAm6Gji9o=";
  };

  nativeBuildInputs = [ dpkg autoPatchelfHook ];

  buildInputs = [
    glib
    libX11
    libxcb
    libXcomposite
    libXcursor
    libXdamage
    libXext
    libXfixes
    libXi
    libXrender
    libXtst
    dbus
    gtk3
    pango
    libXrandr
    libdrm
    cups
    nss
    nspr
    mesa
    libXScrnSaver
    alsa-lib
    cairo
    atk
    gdk-pixbuf
    expat
  ];

  unpackCmd = "dpkg -x $curSrc unpacked";

  installPhase = ''
    mkdir -p $out/bin
    cp -r opt $out
    ln -s $out/opt/TETR.IO/tetrio-desktop $out/bin/tetrio-desktop
    cp -r usr/share $out
    substituteInPlace $out/share/applications/tetrio-desktop.desktop \
      --replace "/opt/TETR.IO/" "$out/opt/TETR.IO/"
  '';
}
