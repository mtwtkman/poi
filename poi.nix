{ mkDerivation, aeson, base, directory, lib, QuickCheck, tasty
, tasty-quickcheck, time
}:
mkDerivation {
  pname = "poi";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base directory time ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base QuickCheck tasty tasty-quickcheck ];
  doHaddock = false;
  doCheck = false;
  license = lib.licenses.wtfpl;
  mainProgram = "poi-cli";
}
