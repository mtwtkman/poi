{ mkDerivation, base, directory, filepath, lib, mtl
, optparse-applicative, QuickCheck, random, regex, tasty
, tasty-quickcheck, text, time, uuid
}:
mkDerivation {
  pname = "poi";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base directory filepath mtl regex text time uuid
  ];
  executableHaskellDepends = [
    base directory filepath optparse-applicative
  ];
  testHaskellDepends = [
    base filepath QuickCheck random tasty tasty-quickcheck time uuid
  ];
  license = "unknown";
  mainProgram = "poi";
}
