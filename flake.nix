{
  description = "shiki: small probes that report back";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      # shiki.nix comes from `cabal2nix . > shiki.nix`. Run it again when
      # shiki.cabal changes.
      withTests = pkgs: pkgs.haskellPackages.callPackage ./shiki.nix { };

      # The executable alone, without the Haskell libraries in its closure.
      # The git probe calls git, so git goes on its PATH. gitMinimal, because
      # the full git brings Python, Perl and the Apple SDK (about 1.6 GB).
      shikiFor = pkgs:
        let
          lib = pkgs.haskell.lib;
          base = lib.justStaticExecutables (lib.dontHaddock (lib.dontCheck (withTests pkgs)));
        in
        lib.overrideCabal base (drv: {
          buildTools = (drv.buildTools or [ ]) ++ [ pkgs.makeWrapper ];
          postInstall = (drv.postInstall or "") + ''
            wrapProgram $out/bin/shiki --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.gitMinimal ]}
          '';
        });
    in
    {
      packages = forAllSystems (pkgs: {
        default = shikiFor pkgs;
        shiki = shikiFor pkgs;
      });

      checks = forAllSystems (pkgs: {
        tests = withTests pkgs;
      });

      homeManagerModules.default = import ./nix/module.nix { inherit self; };

      devShells = forAllSystems (pkgs: {
        default = pkgs.haskellPackages.shellFor {
          packages = _: [ (withTests pkgs) ];
          nativeBuildInputs = with pkgs.haskellPackages; [
            cabal-install
            cabal2nix
            haskell-language-server
            ormolu
          ];
        };
      });
    };
}
