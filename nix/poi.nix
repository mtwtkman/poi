{ mkDerivation, base, lib }:
mkDerivation {
  pname = "poi";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  doHaddock = false;
  license = "unknown";
  mainProgram = "poi";
}
