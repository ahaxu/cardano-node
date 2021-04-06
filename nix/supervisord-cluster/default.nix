{ pkgs
, lib
, bech32
, basePort ? 30000
, stateDir ? "state-cluster"
, cacheDir ? "${__getEnv "HOME"}/.cache"
, extraSupervisorConfig ? {}
, useCabalRun ? false
, enableEKG ? true
##
, profileName ? "default-mary"
, ...
}:
with lib;
let
  backend =
    rec
    { ## First, generic items:
      inherit basePort;
      staggerPorts = true;

      topologyForNode =
        { profile, nodeSpec }:
        let inherit (nodeSpec) name i; in
        pkgs.runWorkbench "topology-${name}.json"
          (if nodeSpec.isProducer
           then "topology for-local-node     ${toString i}   ${profile.topology.files} ${toString basePort}"
           else "topology for-local-observer ${profile.name} ${profile.topology.files} ${toString basePort}");

      finaliseNodeService =
        { name, i, isProducer, ... }: svc: recursiveUpdate svc
          ({
            stateDir       = stateDir + "/${name}";
            ## Everything is local in the supervisord setup:
            socketPath     = "node.socket";
            topology       = "topology.json";
            nodeConfigFile = "config.json";
          } // optionalAttrs useCabalRun {
            executable     = "cabal run exe:cardano-node --";
          } // optionalAttrs isProducer {
            operationalCertificate = "../shelley/node-keys/node${toString i}.opcert";
            kesKey         = "../shelley/node-keys/node-kes${toString i}.skey";
            vrfKey         = "../shelley/node-keys/node-vrf${toString i}.skey";
          });

      finaliseNodeConfig =
        { port, ... }: cfg: recursiveUpdate cfg
          ({
            ShelleyGenesisFile   = "../shelley/genesis.json";
            ByronGenesisFile     =   "../byron/genesis.json";
          } // optionalAttrs enableEKG {
            hasEKG               = port + supervisord.portShiftEkg;
            hasPrometheus        = [ "127.0.0.1" (port + supervisord.portShiftPrometheus) ];
            setupBackends = [
              "EKGViewBK"
            ];
          });

      ## Backend-specific:
      supervisord =
        {
          inherit
            extraSupervisorConfig;

          portShiftEkg        = 100;
          portShiftPrometheus = 200;

          mkSupervisorConf =
            profile:
            pkgs.callPackage ./supervisor-conf.nix
            { inherit (profile) node-services;
              inherit
                pkgs lib stateDir
                basePort
                extraSupervisorConfig;
            };
        };

      ## Actions:
      deploy =
        profile:
        ''
        cat <<EOF
Starting cluster:
  - state dir:       ${stateDir}
  - profile JSON:    ${profile.JSON}
  - node specs:      ${profile.node-specs.JSON}
  - topology:        ${profile.topology.files}/topology-nixops.json ${profile.topology.files}/topology.pdf
  - node port base:  ${toString basePort}
  - EKG URLs:        http://localhost:${toString (basePort + supervisord.portShiftEkg)}/
  - Prometheus URLs: http://localhost:${toString (basePort + supervisord.portShiftPrometheus)}/metrics

EOF

        ${__concatStringsSep "\n"
          (flip mapAttrsToList profile.node-services
            (name: svc:
              ''
              mkdir -p ${stateDir}/${name}
              cp ${svc.nodeSpec.JSON}      ${stateDir}/${name}/spec.json
              cp ${svc.serviceConfig.JSON} ${stateDir}/${name}/service-config.json
              cp ${svc.nodeConfig.JSON}    ${stateDir}/${name}/config.json
              cp ${svc.topology.JSON}      ${stateDir}/${name}/topology.json
              cp ${svc.startupScript}      ${stateDir}/${name}/start.sh
              ''
            ))}

        SUPERVISOR_CONF=${supervisord.mkSupervisorConf profile}
        cp $SUPERVISOR_CONF ${stateDir}/supervisor/supervisord.conf
        ${pkgs.python3Packages.supervisor}/bin/supervisord \
            --config $SUPERVISOR_CONF $@
        '';

      activate =
        profile:
        ''
        ## Wait for socket activation:
        #
        if test ! -v "CARDANO_NODE_SOCKET_PATH"
        then export CARDANO_NODE_SOCKET_PATH=$PWD/${stateDir}/node-0/node.socket; fi
        while [ ! -S $CARDANO_NODE_SOCKET_PATH ]; do echo "Waiting 5 seconds for bft node to start"; sleep 5; done

        echo 'Recording node pids..'
        ${pkgs.psmisc}/bin/pstree -Ap $(cat ${stateDir}/supervisor/supervisord.pid) |
        grep 'cabal.*cardano-node' |
        sed -e 's/^.*-+-cardano-node(\([0-9]*\))-.*$/\1/' \
        > ${stateDir}/supervisor/cardano-node.pids

        ${optionalString (!profile.value.genesis.single_shot)
          ''
          echo "Transfering genesis funds to pool owners, register pools and delegations"
          cli transaction submit \
              --cardano-mode \
              --tx-file ${stateDir}/shelley/transfer-register-delegate-tx.tx \
              --testnet-magic ${toString profile.value.genesis.network_magic}

          sleep 5
          ''}

        echo 'Cluster started. Run `stop-cluster` to stop'
        '';
    };

  environment =
    {
      cardanoLib = pkgs.commonLib.cardanoLib;
      inherit
        stateDir cacheDir
        basePort;
    };

  workbenchProfiles = pkgs.generateWorkbenchProfiles
    { inherit pkgs backend environment; };
  # workbenchPkgs = pkgs.callPackage
  #   ../workbench
  #   { inherit pkgs backend environment; };
in

let
  profile = workbenchProfiles.profiles."${profileName}"
    or (throw "No such profile: ${profileName};  Known profiles: ${toString (__attrNames workbenchProfiles.profiles)}");

  inherit (profile.value) era composition monetary;

  # topology = pkgs.callPackage ./topology.nix
  #   { inherit lib stateDir;
  #     inherit (pkgs) graphviz;
  #     inherit (profile.value) composition;
  #     localPortBase = basePort;
  #   };

  defCardanoExesBash = pkgs.callPackage ./cardano-exes.nix
    { inherit (pkgs) cabal-install cardano-node cardano-cli cardano-topology;
      inherit lib stateDir useCabalRun;
    };

  ## This yields two attributes: 'params' and 'files'
  mkGenesisBash = pkgs.callPackage ./genesis.nix
    { inherit
      lib
      cacheDir stateDir
      basePort
      profile;
    };

  path = makeBinPath
    [ bech32 pkgs.workbench pkgs.jq pkgs.gnused pkgs.coreutils pkgs.bash pkgs.moreutils ];

  start = pkgs.writeScriptBin "start-cluster" ''
    set -euo pipefail

    PATH=$PATH:${path}

    while test $# -gt 0
    do case "$1" in
        --trace | --debug ) set -x;;
        * ) break;; esac; shift; done

    wb supervisor assert-stopped

    wb profile describe ${profileName}

    ${defCardanoExesBash}

    if test -e "${stateDir}" && test ! -f "${stateDir}"/byron-protocol-params.json
    then echo "ERROR: state directory exists, but looks suspicious -- refusing to remove it: '${stateDir}'" >&2
         exit 1
    fi

    rm -rf   "${stateDir}"
    mkdir -p "${stateDir}"/supervisor "${cacheDir}"

    ln -s ${profile.topology.files} "${stateDir}"/topology

    wb genesis prepare ${profile.JSON} ${profile.topology.files} \
       "${stateDir}"/genesis

    ${backend.deploy   profile}
    ${backend.activate profile}
  '';
  stop = pkgs.writeScriptBin "stop-cluster" ''
    set -euo pipefail
    ${pkgs.python3Packages.supervisor}/bin/supervisorctl stop all
    if [ -f ${stateDir}/supervisor/supervisord.pid ]
    then
      kill $(<${stateDir}/supervisor/supervisord.pid) $(<${stateDir}/supervisor/cardano-node.pids)
      echo "Cluster terminated!"
      rm -f ${stateDir}/supervisor/supervisord.pid ${stateDir}/supervisor/cardano-node.pids
    else
      echo "Cluster is not running!"
    fi
  '';

in
{
  inherit profile start stop;
}
