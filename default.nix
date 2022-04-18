{ compiler ? "ghc8107" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "hlog" =
        hself.callCabal2nix
          "hlog"
          (gitignore ./.)
          {};
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."hlog"
    ];
    buildInputs = [
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ormolu
      pkgs.haskellPackages.hlint
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };
in
{
  inherit shell;
  inherit myHaskellPackages;
  "hlog" = myHaskellPackages."hlog";
}
