{ pkgs, ... }:
let
  startDb = pkgs.writeScriptBin "startDb" ''
    initdb -D $PGDATA

    pg_ctl                                                  \
      -D $PGDATA                                            \
      -l $PGDATA/postgres.log                               \
      -o "-c unix_socket_directories='$PGDATA'"             \
      -o "-c listen_addresses='*'"                          \
      -o "-c log_destination='stderr'"                      \
      -o "-c logging_collector=on"                          \
      -o "-c log_directory='log'"                           \
      -o "-c log_filename='postgresql-%Y-%m-%d_%H%M%S.log'" \
      -o "-c log_min_messages=info"                         \
      -o "-c log_min_error_statement=info"                  \
      -o "-c log_connections=on"                            \
      -o "-F -p 5321"                                       \
      start

    echo "create postgres user"
    createuser postgres -s --host $PGDATA -p 5321     
      
    echo "initializing Changeblock Dev & Test db"
    createdb cbl --host $PGDATA -p 5321
    createdb cbl_test --host $PGDATA -p 5321
      
  '';

  resetTestDb = pkgs.writeScriptBin "resetTestDb" ''
    dropdb cbl_test --host $PGDATA
    createdb cbl_test --host $PGDATA
  '';

in
{
  # name = "project-name";
  compiler-nix-name = "ghc965"; # Version of GHC to use

  # Cross compilation support:
  # crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
  #   p.mingwW64
  #   p.ghcjs
  # ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
  #   p.musl64
  # ]);

  # Tools to include in the development shell
  shell = {
    tools = {
      cabal = {};
      hlint = "3.8";
      haskell-language-server = "2.9.0.0";
    };

    buildInputs = with pkgs; [
      jq
      haskellPackages.cabal-fmt
      haskellPackages.fourmolu
      nixpkgs-fmt
      gnumake
      startDb
      awsebcli
      awscli
      postgresql_14
    ];


    nativeBuildInputs = with pkgs; [
      pkg-config
    ];


    # Start and terminate postgresql gracefully
    shellHook = ''
      export PG_CONFIG=$(which pg_config)
      mkdir -p $PWD/.db
      export PGDATA=$PWD/.db

      export DB_CONNECTION=postgres://postgres:postgres@localhost:5321/cbl
      export DB_CONNECTION_TEST=postgres://postgres:postgres@localhost:5321/cbl_test

      startDb

      trap \
        "
          # Stop PostgreSQL
          pg_ctl -D $PGDATA stop
        " \
      EXIT


    '';
  };
}
