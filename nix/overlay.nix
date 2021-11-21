final:
previous:
with final.haskell.lib;
{
  cursorFuzzyTimePackages =
    let cursorFuzzyTimePkg = name:
      (buildStrictly (final.haskellPackages.callCabal2nixWithOptions name (../. + "/${name}") "--no-hpack" { }));
    in
    final.lib.genAttrs [
      "cursor-fuzzy-time"
      "cursor-fuzzy-time-gen"
      "cursor-fuzzy-time-demo"
    ]
      cursorFuzzyTimePkg;
  haskellPackages = previous.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super: final.cursorFuzzyTimePackages
    );
  });
}
