{pkgs, ...}: rec {
  name = "reticule-minor";
  compiler-nix-name = "ghc925";
  index-state = "2023-01-22T00:00:00Z";
  modules = [
    {
      packages.gl.components.library = {
        libs = pkgs.lib.mkForce [pkgs.libGL];
        doHaddock = false;
      };
      packages.OpenGLRaw.components.library.doHaddock = false;
      enableProfiling = true;
    }
  ];

  shell = {
    withHoogle = true;
    tools.cabal = {inherit index-state; };
    tools.cabal-fmt = {inherit index-state; };
    tools.hlint = {inherit index-state; };
    tools.haskell-language-server = {inherit index-state; };
  };
}
