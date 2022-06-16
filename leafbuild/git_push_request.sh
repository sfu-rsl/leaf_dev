#!/bin/bash

# SPDX-License-Identifier
# Copyright (C) 2021-2022 Simon Fraser University (www.sfu.ca)

set -euxo pipefail

if (( "$#" >= 1 )); then
    msg="$1"; shift
else
    msg="$(git log -1 --pretty=format:%B)"
fi

branch_main=main
branch_mirror="$(git branch --contains "$(git log -1 --pretty=format:%H)" | grep '*' | cut -d ' ' -f 2-)"

if [ "$branch_mirror" = "$branch_main" ]; then
    echo "Error: current branch should not be '$branch_main'" >&2
    exit 1
fi

git push --force-with-lease
req_nb="$(hub pull-request -m "$msg" | tr '/' '\n' | tail -n 1)"
github_user=$(grep -m 1 user ~/.config/hub | cut -d ' ' -f 3-)

function echo_read () {
    set +x
    echo -n "$1"
    read
    set -x
}

function rebase_continue () {
    echo_read 'Conflict resolved? '
    git rebase --continue
}

while true; do
    git checkout "$branch_main"
    git merge --no-ff -m "Merge pull request #${req_nb} from ${github_user}/${branch_main}" -m "$msg" "$branch_mirror"
    
    declare -i err=0
    git push || err="$?"
    if (( "$err" == 0 )); then
        break
    fi
    
    git checkout "$branch_mirror"
    git branch -f "$branch_main" HEAD~
    git checkout "$branch_main"
    git pull
    git checkout "$branch_mirror"
    git rebase "$branch_main" || rebase_continue
    
    git push --force-with-lease
done

git branch -f "$branch_mirror"
git checkout "$branch_mirror"
