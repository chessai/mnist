{ package ? "mnist", compiler ? "ghc822" }:

(import ./default.nix {
  inherit package compiler;
}).mnist
