{ cabal, cabalInstall, lens, mtl, transformers, tasty, tastyQuickcheck }:

cabal.mkDerivation (self: {
  pname = "haskell-space-trader";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildTools = [ cabalInstall ];
  buildDepends = [ lens mtl transformers ];
  testDepends = [ tasty tastyQuickcheck ];
  meta = {
    platforms = self.ghc.meta.platforms;
  };
})
