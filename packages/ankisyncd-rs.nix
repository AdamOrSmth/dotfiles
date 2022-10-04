# I'm far too lazy to properly build this, let's just use the binary and call it a day

{ stdenv, fetchurl, autoPatchelfHook, openssl_1_1 }:

let
  pname = "anki-sync-server-rs";
  version = "0.2.1";
in stdenv.mkDerivation {
  inherit pname version;

  src = fetchurl {
    url =
      "https://github.com/ankicommunity/anki-sync-server-rs/releases/download/${version}/ankisyncd_${version}_linux_x64.tar.gz";
    sha256 = "1mx2vadkqj0rfq9r3z3ipd2cfags6wi5fcp03zdvz8s7hjk2b0qc";
  };

  nativeBuildInputs = [ autoPatchelfHook ];

  buildInputs = [ openssl_1_1 ];

  installPhase = ''
    mkdir -p $out/bin
    cp * $out
    ln -s $out/ankisyncd $out/bin
  '';
}
