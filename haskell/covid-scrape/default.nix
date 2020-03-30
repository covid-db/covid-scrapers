{ compiler ? "ghc881"
, rev      ? "c4f97342ba8ac84def72328616dd05d005bb4715"
, sha256   ? "1p2gbisib2jrz4r9b5vzfvmirgmz9sr2ksalngaw908vvg9hsvai"
, pkgs     ?
    import (builtins.fetchTarball {
      url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      inherit sha256; }) {
      config.allowBroken = false;
      config.allowUnfree = true;
    }
}:
let gitignoreSrc = import (pkgs.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "gitignore";
      rev = "2ced4519f865341adcb143c5d668f955a2cb997f";
      sha256 = "0fc5bgv9syfcblp23y05kkfnpgh3gssz6vn24frs8dzw39algk2z";
    }) {};
    beamSrc = pkgs.fetchFromGitHub {
      owner = "tathougies";
      repo = "beam";
      rev = "ff6d16daa189355db4cf24e90d8173768c1418bb";
      sha256 = "11f1nrw3s7ihf3lyskjx1mdfi4s5d3rfn0fwwmcc8xl2dgjdlnk8";
    };

in
pkgs.haskell.packages.${compiler}.developPackage {
  name = builtins.baseNameOf ./.;
  root = gitignoreSrc.gitignoreSource ./.;

  overrides = self: super: with pkgs.haskell.lib; {
    beam-core = dontCheck super.beam-core;
    beam-migrate = doJailbreak super.beam-migrate;
    beam-postgres = dontCheck super.beam-postgres;

    # Get a specific hackage version straight from hackage. Unlike the above
    # callHackage approach, this will always succeed if the version is on
    # hackage. The downside is that you have to specify the hash manually.
    # aeson = callHackageDirect {
    #   pkg = "aeson";
    #   ver = "1.4.2.0";
    #   sha256 = "0qcczw3l596knj9s4ha07wjspd9wkva0jv4734sv3z3vdad5piqh";
    # } {};
    #
    # To discover more functions that can be used to modify haskell
    # packages, run "nix-repl", type "pkgs.haskell.lib.", then hit
    # <TAB> to get a tab-completed list of functions.
  };
  source-overrides = {
    beam-core = "${beamSrc}/beam-core";
    beam-migrate = "${beamSrc}/beam-migrate";
    beam-postgres = "${beamSrc}/beam-postgres";

    haskell-src-exts = "1.21.1";

    # Use a particular commit from github
    tagsoup-parsec = pkgs.fetchFromGitHub
      { owner = "mightybyte";
        repo = "tagsoup-parsec";
        rev = "e87a6b00b1a49e8cea6c4e26e732806d3e782ef3";
        sha256 = "01siwipdznjdqpkflwingq89kp8knbhxwq3wi13r1l41p7vwfq8a";
      };
  };
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      pkgs.haskell.packages.${compiler}.cabal-install
      pkgs.haskell.packages.${compiler}.ghcid
    ];
  });
}
