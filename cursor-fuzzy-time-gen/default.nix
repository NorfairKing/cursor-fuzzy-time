{ mkDerivation, base, containers, criterion, cursor-fuzzy-time
, cursor-gen, deepseq, fuzzy-time, fuzzy-time-gen, genvalidity
, genvalidity-criterion, genvalidity-hspec
, genvalidity-hspec-optics, genvalidity-time, hspec, lib
, megaparsec, QuickCheck, text, time
}:
mkDerivation {
  pname = "cursor-fuzzy-time-gen";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers cursor-fuzzy-time cursor-gen deepseq fuzzy-time
    fuzzy-time-gen genvalidity genvalidity-time megaparsec time
  ];
  testHaskellDepends = [
    base cursor-fuzzy-time fuzzy-time genvalidity genvalidity-hspec
    genvalidity-hspec-optics genvalidity-time hspec megaparsec
    QuickCheck text time
  ];
  benchmarkHaskellDepends = [
    base criterion cursor-fuzzy-time genvalidity-criterion QuickCheck
  ];
  homepage = "https://github.com/NorfairKing/fuzzy-time";
  license = lib.licenses.mit;
}
