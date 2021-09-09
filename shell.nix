{ pkgs ? import <nixpkgs> {}
, system ? builtins.currentSystem
}:
( import ./nix.nix { inherit pkgs system; } ).shell
