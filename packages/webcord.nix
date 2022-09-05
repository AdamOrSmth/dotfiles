{ lib, appimageTools, fetchurl, }:

let version = "3.8.1";
in appimageTools.wrapType2 {
  pname = "webcord";
  inherit version;

  src = fetchurl {
    url =
      "https://github.com/SpacingBat3/WebCord/releases/download/v${version}/WebCord-${version}-x64.AppImage";
    sha256 = "0r8dqpcci0ig7mfhnlqgz614gv183w9s0lsclhwfgkbi5n0zpxzp";
  };
}
