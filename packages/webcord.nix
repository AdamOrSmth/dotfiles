{ lib, appimageTools, fetchurl, }:

let version = "3.8.3";
in appimageTools.wrapType2 {
  pname = "webcord";
  inherit version;

  src = fetchurl {
    url =
      "https://github.com/SpacingBat3/WebCord/releases/download/v${version}/WebCord-${version}-x64.AppImage";
    sha256 = "05xw6s0l99lzbvrpx8c76c1zckm1qljxqz1rsfp2m1nsq9j2xafa";
  };
}
