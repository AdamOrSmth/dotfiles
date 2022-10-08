{ stdenv, fetchFromGitHub, cmake, pkg-config, libuv, libsodium, unbound, zeromq
, sqlite, openssl, libevent, curl, jemalloc, libcap }:

stdenv.mkDerivation rec {
  pname = "lokinet";
  version = "0.9.9";

  src = fetchFromGitHub {
    owner = "oxen-io";
    repo = "lokinet";
    rev = "v${version}";
    sha256 = "AaGsRg9S9Cng9emI/mN09QSOIRbE+x3916clWAwLnRs=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ cmake pkg-config ];
  buildInputs = [
    libuv
    libsodium
    unbound
    zeromq
    sqlite
    openssl
    libevent
    curl
    jemalloc
    libcap
  ];

  cmakeFlags = [ "-DWITH_LTO=OFF" ];
}
