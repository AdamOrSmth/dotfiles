{ onlykey-agent, python3Packages, fetchFromGitHub, libnotify }:

let
  inherit (python3Packages) buildPythonPackage fetchPypi;
  onlykey = let
    pname = "onlykey";
    version = "1.2.9";
  in buildPythonPackage {
    inherit pname version;

    src = fetchPypi {
      inherit pname version;
      sha256 = "92CzDZgtmww0eABtjeBo6HNQ00sijWakjXLPJiOXY/A=";
    };

    propagatedBuildInputs = with python3Packages; [
      pynacl
      aenum
      cython
      prompt-toolkit
      hidapi
      ecdsa
      onlykey-solo-python
    ];
  };
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

    patches = [ ./patches/onlykey-agent-notifications.patch ];

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

  propagatedBuildInputs = [ onlykey lib-agent ];
})
