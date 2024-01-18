{ mkDerivation, base, criterion, cursor-fuzzy-time, cursor-gen
, fuzzy-time, fuzzy-time-gen, genvalidity, genvalidity-criterion
, genvalidity-hspec, genvalidity-hspec-optics, genvalidity-time
, hspec, lib, QuickCheck, time
}:
mkDerivation {
  pname = "cursor-fuzzy-time-gen";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base cursor-fuzzy-time cursor-gen fuzzy-time-gen genvalidity
    genvalidity-time
  ];
  testHaskellDepends = [
    base cursor-fuzzy-time fuzzy-time genvalidity-hspec
    genvalidity-hspec-optics hspec QuickCheck time
  ];
  benchmarkHaskellDepends = [
    base criterion cursor-fuzzy-time genvalidity-criterion
  ];
  homepage = "https://github.com/NorfairKing/fuzzy-time";
  license = lib.licenses.mit;
}
