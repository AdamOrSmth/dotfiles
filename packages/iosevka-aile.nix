{ lib, fetchzip }:

let version = "15.1.0";
in fetchzip {
  name = "iosevka-aile-${version}";

  url =
    "https://github.com/be5invis/Iosevka/releases/download/v${version}/ttf-iosevka-aile-${version}.zip";

  postFetch = ''
    mkdir -p $out/share/fonts/
    unzip -j $downloadedFile \*.ttf -d $out/share/fonts/truetype
  '';

  sha256 = "CklVAGMAAn19sI6yWPcPbsVo3Wly97tdjdSjNoxM0u0=";
}
