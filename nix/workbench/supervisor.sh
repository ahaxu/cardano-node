usage_supervisor() {
     usage "supervisor" "Managing local cluster" <<EOF
    assert-stopped   Assert that 'supervisord' is not running
    is-running       Test if 'supervisord' is running
EOF
}

supervisor() {
local op=${1:-$(usage_supervisor)}; shift

case "${op}" in
    assert-stopped )
        supervisor is-running &&
          fail "Supervisord is already running. Please run 'stop-cluster' first!" ||
          true;;
    is-running )
        test "$(netstat -pltn 2>/dev/null | grep ':9001 ' | wc -l)" != "0";;


    * ) usage_supervisor;; esac
}
