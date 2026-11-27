{
  description = "Orquestrador de Automacao Windows (PA / IPED)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hPkgs = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            shared = hself.callCabal2nix "shared" ./shared { };
            orchestrator = hself.callCabal2nix "orchestrator" ./orchestrator { };
          };
        };
      in
      {
        packages = {
          shared = hPkgs.shared;
          orchestrator = hPkgs.orchestrator;
          default = hPkgs.orchestrator;
        };

        devShells.default = hPkgs.shellFor {
          packages = p: [ p.shared p.orchestrator ];
          buildInputs = with pkgs; [
            cabal-install
            haskell-language-server
            sqlite
            pkg-config
          ];
        };
      });
}