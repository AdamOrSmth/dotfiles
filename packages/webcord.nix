{ lib, appimageTools, fetchurl, }:

let version = "3.8.4";
in appimageTools.wrapType2 {
  pname = "webcord";
  inherit version;

  # Executable name
  name = "webcord";

  src = fetchurl {
    url =
      "https://github.com/SpacingBat3/WebCord/releases/download/v${version}/WebCord-${version}-x64.AppImage";
    sha256 = "1v31n67221idj5bcw4yrchwq3kl78pkawyms4n5qxh4hqf6jbfwi";
  };

  # For screensharing
  extraPkgs = pkgs: [ pkgs.pipewire ];
}
