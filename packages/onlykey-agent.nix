{ python3Packages, fetchFromGitHub, libnotify, onlykey-agent, my }:

let
  inherit (python3Packages) buildPythonPackage fetchPypi;
  bech32 = let
    pname = "bech32";
    version = "1.2.0";
  in buildPythonPackage {
    inherit pname version;

    src = fetchPypi {
      inherit pname version;
      sha256 = "fW24IUYDvXhx/PpsCCbvaLhbCr2Q+iHChanF4h0r2Jk=";
    };
  };
  lib-agent = let
    pname = "lib-agent";
    version = "1.0.5";
  in buildPythonPackage {
    inherit pname version;

    src = fetchPypi {
      inherit pname version;
      sha256 = "Y9KBr7mX0/5bZg2/+ZjwFbjkC//uJIVQYwCKuOf03v8=";
    };

    propagatedBuildInputs = builtins.attrValues {
      inherit (python3Packages)
        cryptography docutils python-daemon wheel ecdsa pynacl mnemonic pymsgbox
        semver unidecode pycryptodome backports-shutil-which configargparse;
      inherit bech32;
      inherit libnotify;
    };

    doCheck = false;
  };
  version = "1.1.14";
in onlykey-agent.overrideAttrs (oldAttrs: {
  inherit version;

  src = fetchFromGitHub {
    owner = "trustcrypto";
    repo = "onlykey-agent";
    rev = "v${version}";
    sha256 = "tc6UrF8llPvY2rWlSXhyIKR8G4NKG0DjTyRyhjxUj1M=";
  };

  propagatedBuildInputs = [ my.onlykey-cli lib-agent ];
})
