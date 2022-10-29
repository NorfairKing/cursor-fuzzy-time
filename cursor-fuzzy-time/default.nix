{ mkDerivation, base, containers, cursor, deepseq, fuzzy-time, lib
, megaparsec, microlens, text, time, validity, validity-time
}:
mkDerivation {
  pname = "cursor-fuzzy-time";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers cursor deepseq fuzzy-time megaparsec microlens text
    time validity validity-time
  ];
  homepage = "https://github.com/NorfairKing/fuzzy-time";
  license = lib.licenses.mit;
}
