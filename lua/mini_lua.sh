#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

PRELUDE=$(cat << EOF
    coroutine.mini_resume = function(co, x)
        local b, y = coroutine.resume(co, x);
        if not b then error(y) end;
        return y
    end
EOF
)

$SCRIPT_DIR/src/lua -e"$PRELUDE" "$@"
