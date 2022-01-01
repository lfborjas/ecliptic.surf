{ mkDerivation, base, containers, directory, fetchgit, foldl, hspec
, lib, microlens, QuickCheck, random, streaming, swiss-ephemeris
, time, vector
}:
mkDerivation {
  pname = "almanac";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lfborjas/almanac";
    sha256 = "0736ymywp727x5ksi1zljxiyfjx1x9ivqg8x74rsqasikccfn5n7";
    rev = "72931bd9accefccd2df59d9d64a6cbb91f2d5f17";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base containers foldl streaming swiss-ephemeris time vector
  ];
  testHaskellDepends = [
    base containers directory hspec microlens QuickCheck random
    swiss-ephemeris time
  ];
  description = "utilities for ephemeris analysis";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
