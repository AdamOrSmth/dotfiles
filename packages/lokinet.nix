{ stdenv, fetchFromGitHub, cmake, pkg-config, libuv, libsodium, unbound, zeromq
, sqlite, openssl, libevent, curl, jemalloc, libcap }:

stdenv.mkDerivation rec {
  pname = "lokinet";
  version = "0.9.8";

  src = fetchFromGitHub {
    owner = "oxen-io";
    repo = "lokinet";
    rev = "v${version}";
    sha256 = "UlBVoDV6aLKpt59hLVqUqYgS7+gQcnUl5aOU8vOAszA=";
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
