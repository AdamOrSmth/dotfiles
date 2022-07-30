{ onlykey-cli, python3Packages }:

let version = "1.2.10";
in onlykey-cli.overrideAttrs (oldAttrs: {
  src = python3Packages.fetchPypi {
    pname = "onlykey";
    inherit version;
    sha256 = "ZmQnyZx9YlIIxMMdZ0U2zb+QANfcwrtG7iR1LpgzmBQ=";
  };
})
