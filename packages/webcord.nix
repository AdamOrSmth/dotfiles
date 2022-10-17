{ lib, appimageTools, fetchurl, }:

let version = "3.8.8";
in appimageTools.wrapType2 {
  pname = "webcord";
  inherit version;

  # Executable name
  name = "webcord";

  src = fetchurl {
    url =
      "https://github.com/SpacingBat3/WebCord/releases/download/v${version}/WebCord-${version}-x64.AppImage";
    sha256 = "0c2jjmfnwfbmkr3ml1fj0vvxhszpym6q4cmlp205w6hqab1cxnpv";
  };

  # Screensharing on Wayland
  extraPkgs = pkgs: [ pkgs.pipewire ];
}
