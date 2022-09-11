{ lib, appimageTools, fetchurl, }:

let version = "3.8.2";
in appimageTools.wrapType2 {
  pname = "webcord";
  inherit version;

  src = fetchurl {
    url =
      "https://github.com/SpacingBat3/WebCord/releases/download/v${version}/WebCord-${version}-x64.AppImage";
    sha256 = "1h59cv9b543d9wdmsr6ns1i2fz2mrhwxzaxfp0dacsgg62lc40fz";
  };
}
