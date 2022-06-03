#!/bin/bash

# SPDX-License-Identifier
# Copyright (C) 2021-2022 Simon Fraser University (www.sfu.ca)

set -euo pipefail

# resolve home directory of the script
HOME1="$(dirname "$(python3 -c "import os; print(os.path.realpath('$0'))")")"

# resolve home directory of the root project
err=0; HOME0="$(git rev-parse --show-toplevel)" || err="$?"

if (( $err )); then
    if (( $# >= 1 )) ; then
        HOME0="$1"; shift
    else
        exit "$err"
    fi
fi    

name="$(basename "$HOME0")_log_$(git log -1 --pretty=format:%H || true)__"

function tee_log () {
    fic="$HOME0/../$name$(date '+%F_%T' | tr -d ':-')"
    if [ ! -f "$fic" ] ; then
        tee "$fic"
        date -R >> "$fic"
    else
        exit 1
    fi
}

#

sudo "$HOME1/build_all_sudo.sh" "$HOME0" "$HOME1" 2>&1 | tee_log
