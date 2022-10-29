final: prev:
with final.lib;
with final.haskell.lib;
{
  cursorFuzzyTimeRelease =
    final.symlinkJoin {
      name = "cursor-fuzzy-time-release";
      paths = attrValues final.haskellPackages.cursorFuzzyTimePackages;
    };

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: { })) (
      self: super:
        let cursorFuzzyTimePackages =
          let cursorFuzzyTimePkg = name: buildStrictly (self.callPackage (../${name}) { });
          in
          final.lib.genAttrs [
            "cursor-fuzzy-time"
            "cursor-fuzzy-time-gen"
            "cursor-fuzzy-time-demo"
          ]
            cursorFuzzyTimePkg;
        in { inherit cursorFuzzyTimePackages; } // cursorFuzzyTimePackages
    );
  });
}
