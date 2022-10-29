{ mkDerivation, base, brick, cursor, cursor-fuzzy-time, directory
, fuzzy-time, lib, microlens, text, time, vty
}:
mkDerivation {
  pname = "cursor-fuzzy-time-demo";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base brick cursor cursor-fuzzy-time directory fuzzy-time microlens
    text time vty
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/picosmos#readme";
  license = lib.licenses.mit;
  mainProgram = "cursor-fuzzy-time-demo";
}
