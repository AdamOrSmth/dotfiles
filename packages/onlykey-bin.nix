{ lib, stdenv, fetchurl, dpkg, autoPatchelfHook, glib, nss, nspr, libX11, libxcb
, libXcomposite, libXcursor, libXdamage, libXext, libXfixes, libXi, libXrender
, libXtst, cups, dbus, libXScrnSaver, expat, util-linux, libXrandr, alsa-lib
, pango, cairo, atk, at-spi2-core, gtk3, gdk-pixbuf, libudev0-shim, makeWrapper
}:

let version = "5.3.6";
in stdenv.mkDerivation {
  pname = "onlykey-bin";
  inherit version;

  src = fetchurl {
    url =
      "https://github.com/trustcrypto/OnlyKey-App/releases/download/v${version}/OnlyKey_${version}_amd64.deb";
    sha256 = "1eIjWBtFnIaaLy1MhM139SuFHc7VyH3qX42T7Bzsx8Q=";
  };

  nativeBuildInputs = [ dpkg autoPatchelfHook makeWrapper ];

  buildInputs = [
    glib
    nss
    nspr
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
    cups
    dbus
    libXScrnSaver
    expat
    util-linux
    libXrandr
    alsa-lib
    pango
    cairo
    atk
    at-spi2-core
    gtk3
    gdk-pixbuf

    libudev0-shim
  ];

  unpackCmd = "dpkg -x $curSrc unpacked";

  installPhase = ''
    mkdir -p $out/bin
    cp -r opt $out
    ln -s $out/opt/OnlyKey/nw $out/bin/onlykey
    cp -r usr/share $out
    substituteInPlace $out/share/applications/OnlyKey.desktop \
      --replace "/opt/OnlyKey/" "$out/opt/OnlyKey/"
  '';

  preFixup = let runtimeLibs = lib.makeLibraryPath [ libudev0-shim ];
  in ''
    wrapProgram "$out/opt/OnlyKey/nw" --prefix LD_LIBRARY_PATH : ${runtimeLibs}
  '';
}
