{ onlykey-cli, python3Packages }:

let version = "1.2.9";
in onlykey-cli.overrideAttrs (oldAttrs: {
  src = python3Packages.fetchPypi {
    pname = "onlykey";
    inherit version;
    sha256 = "92CzDZgtmww0eABtjeBo6HNQ00sijWakjXLPJiOXY/A=";
  };
})
