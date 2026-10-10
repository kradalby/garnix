{ pkgs, flakeInputs, ... }:
let
  fixtures = pkgs.runCommand "custom-gc-fixtures" { } ''
    mkdir -p $out/bin
    for command in df mount nix-env nix-collect-garbage nix-heuristic-gc pgrep; do
      printf '#!${pkgs.python3}/bin/python3\n' > $out/bin/$command
      cat ${./custom-gc-command.py} >> $out/bin/$command
      chmod +x $out/bin/$command
    done
    ln -s ${pkgs.coreutils}/bin/tr $out/bin/tr
  '';
  testPkgs = pkgs // {
    coreutils = fixtures;
    nix = fixtures;
    nix-heuristic-gc = fixtures;
    util-linux = fixtures;
    procps = fixtures;
  };
  script =
    useNixHeuristicGc:
    let
      evaluation = flakeInputs.nixpkgs.lib.nixosSystem {
        system = pkgs.stdenv.hostPlatform.system;
        modules = [
          (args: import ../modules/custom-gc.nix (args // { pkgs = testPkgs; }))
          {
            nix = {
              enable = false;
              package = fixtures;
            };
            garnix.custom-gc = {
              enable = true;
              enableTimer = true;
              targetPercent = 90;
              maxIterations = 3;
              inherit useNixHeuristicGc;
            };
          }
        ];
      };
    in
    evaluation.config.systemd.services.custom-gc.script;
in
pkgs.runCommand "custom-gc-test" { nativeBuildInputs = [ pkgs.python3 ]; } ''
  python3 ${./custom-gc-test.py} ${script false} ${script true}
  touch $out
''
