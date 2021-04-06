{ lib
, stdenv
, graphviz
, jq
, moreutils
, makeWrapper
, runCommand

, cardano-cli
, cardano-topology
}:

with lib;

let
  workbench =
    stdenv.mkDerivation {
      pname = "workbench";

      version = "0.1";

      src = ./.;

      buildInputs = [ jq makeWrapper ];

      buildPhase = ''
        patchShebangs .
      '';

      postFixup = ''
        wrapProgram "$out/bin/wb" --prefix PATH ":" ${stdenv.lib.makeBinPath
          [ graphviz
            jq
            moreutils

            cardano-cli
            cardano-topology
          ]}
      '';

      installPhase = ''
        mkdir -p         $out/bin
        cp -a wb profiles *.sh *.jq $out/bin
      '';

      dontStrip = true;
    };

  runWorkbench =
    name: command:
    runCommand name {} ''
      ${workbench}/bin/wb ${command} > $out
    '';

  runJq =
    name: args: query:
    runCommand name {} ''
      args=(${args})
      ${jq}/bin/jq '${query}' "''${args[@]}" > $out
    '';

  generateWorkbenchProfiles =
    { pkgs

    ## The backend is an attrset of AWS/supervisord-specific methods and parameters.
    , backend

    ## Environmental settings:
    ##   - either affect semantics on all backends equally,
    ##   - or have no semantic effect
    , environment
    }:
    rec {
      profile-names-json =
        runWorkbench "profile-names.json" "profiles list";

      profile-names =
        __fromJSON (__readFile profile-names-json);

      mkProfile =
        profileName:
        pkgs.callPackage ./profiles
          { inherit
              pkgs
              backend
              environment
              profileName;
          };

      profiles = genAttrs profile-names mkProfile;
    };
in
{
  inherit workbench runWorkbench runJq;

  inherit generateWorkbenchProfiles;
}
