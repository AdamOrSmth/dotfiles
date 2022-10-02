{ lib, appimageTools, fetchurl, }:

let version = "3.8.6";
in appimageTools.wrapType2 {
  pname = "webcord";
  inherit version;

  # Executable name
  name = "webcord";

  src = fetchurl {
    url =
      "https://github.com/SpacingBat3/WebCord/releases/download/v${version}/WebCord-${version}-x64.AppImage";
    sha256 = "1k7svhww1z2ix77x94zi980b2vnhcvwgnmp5zrkpjccx63nsk6pm";
  };

  # For screensharing
  extraPkgs = pkgs: [ pkgs.pipewire ];
}
