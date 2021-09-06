{ pkgs }:

let

purs-nix =
  import
    (builtins.fetchGit
       { url = "https://github.com/ursi/purs-nix.git";
         rev = "988505248316b1dc82504c8e25358da535e34bd6";
       }
    ) {};

nixed = purs-nix.purs
  { srcs = [ ./src ];
    dependencies =
      with purs-nix.ps-pkgs;
      [
        effect
        lists
        arrays
        maybe
        either
        aff
        aff-promise
        argonaut-core
        argonaut-codecs
        argonaut-generic
        optparse
        node-fs
        debug
      ];
  };

gitignoreSource =
  let src = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore.nix";
    rev = "211907489e9f198594c0eb0ca9256a1949c9d412";
    sha256 = "06j7wpvj54khw0z10fjyi31kpafkr6hi1k0di13k1xp8kywvfyx8";
  };
  in (import src { inherit (pkgs) lib; }).gitignoreSource;

in {

  deriv = pkgs.stdenv.mkDerivation {
    name = "hours";
    src = gitignoreSource ./.;

    installPhase = ''
      mkdir -p $out/bin
      echo "${pkgs.nodejs}/bin/node ${nixed.modules.Main.bundle {}} \"\$@\"" > $out/bin/hours
      chmod +x $out/bin/hours
    '';
  };

  shell = pkgs.mkShell {
    buildInputs =
      [ (nixed.command {
          srcs = [ "$PWD/src" ];
        })
        pkgs.nodejs
      ];

    shellHook = ''
    '';
  };

}
