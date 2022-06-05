{ stdenv, fetchFromGitLab }:

stdenv.mkDerivation {
  name = "shell-color-scripts";

  src = fetchFromGitLab {
    owner = "dwt1";
    repo = "shell-color-scripts";
    rev = "fcd013ea2e1ff80e01adbcea9d0eaf9c73db94c0";
    sha256 = "bd3NBf99rCiADUKQb6fzCBDaKBmYaZHcO4qokm/39do=";
  };

  installPhase = ''
    mkdir -p $out/bin
    cp -r colorscripts $out/
    cp colorscript.sh $out/bin/colorscript
    substituteInPlace $out/bin/colorscript --replace /opt/shell-color-scripts/colorscripts $out/colorscripts
  '';
}
