{ pkgs
, system ? builtins.currentSystem
}:

let

purs-nix =
  import
    (builtins.fetchGit
       { url = "https://github.com/ursi/purs-nix.git";
         rev = "b05de5ddf8318bc87b6820a95d414140ed8cc203";
       }
    ) { inherit system; };

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

uglify-js =
  let ujs-src = pkgs.fetchFromGitHub {
          owner = "mishoo";
          repo = "UglifyJS";
          rev = "70ceda5398535c7028682e05cc8b82009953e54d";
          sha256 = "09gnmmzwzn06lshnv5vp6hai2v8ngsjn3c76rf1d7c4nzlrn2w3p";
        };
  in pkgs.writeShellScript "uglifyjs" ''
        ${pkgs.nodejs}/bin/node ${ujs-src}/bin/uglifyjs "$@"
      '';

result-js =
  pkgs.runCommand "hours-ugly" {}
    "${uglify-js} ${nixed.modules.Main.bundle {}} -c toplevel -m -o $out";

in {

  deriv = pkgs.stdenv.mkDerivation {
    name = "hours";
    src = gitignoreSource ./.;

    installPhase = ''
      mkdir -p $out/bin
      echo "${pkgs.nodejs}/bin/node ${result-js} \"\$@\"" > $out/bin/hours
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
