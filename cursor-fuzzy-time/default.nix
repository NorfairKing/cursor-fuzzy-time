{ mkDerivation, base, cursor, deepseq, fuzzy-time, lib, megaparsec
, microlens, text, time, validity
}:
mkDerivation {
  pname = "cursor-fuzzy-time";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base cursor deepseq fuzzy-time megaparsec microlens text time
    validity
  ];
  homepage = "https://github.com/NorfairKing/fuzzy-time";
  license = lib.licenses.mit;
}
