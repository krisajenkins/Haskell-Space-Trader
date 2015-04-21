{ cabal, cabalInstall }:

cabal.mkDerivation (self: {
  pname = "htext";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildTools = [ cabalInstall ];
  buildDepends = [];
  meta = {
    platforms = self.ghc.meta.platforms;
  };
})
