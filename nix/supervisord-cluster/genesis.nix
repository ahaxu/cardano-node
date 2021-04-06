{ lib
, runCommand
, writeText
##
, cacheDir
, stateDir
, basePort
##
, profile
}:

with profile.value;

let
  hardcodedDefaultUtxoCredentials =
    dir:
    ''
    cp ${./genesis-utxo.vkey} ${dir}/genesis-utxo.vkey
    cp ${./genesis-utxo.skey} ${dir}/genesis-utxo.skey
    '';
in
''
    mkdir -p ${genesisCacheDir}

    ${decideSystemStart}

    ${byronGenesis}

    ${shelleyGenesis}

    echo "Generated genesis for ${name} (cache id $genesis_cache_id)"
''
