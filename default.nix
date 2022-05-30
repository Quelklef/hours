{ pkgs
  ? import
      (builtins.fetchTarball
         { url = "https://github.com/NixOS/nixpkgs/archive/33772708c6d0e33f697426ba386aa0149cbcbecb.tar.gz";
           sha256 = "0l9s56vbh9ljndc1g3fm2byzfwdc0d1gjh21lqyvnjqzqwbc0gfm";
         }
      )
      { inherit system; }
, system ? builtins.currentSystem
}:
( import ./nix.nix { inherit pkgs system; } ).deriv
