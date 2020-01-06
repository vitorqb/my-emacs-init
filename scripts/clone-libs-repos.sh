#!/bin/bash
# Clones personal git repositories needed for emacs-init.el
# If an argument is passed, clones to that directory. Defaults to
# ~/.emacs.d/other/

DEST=${1:-~/.emacs.d/other/}

function extract_name_from_row() { echo "$1" | awk '{print $1}' ; }
function extract_repo_from_row() { echo "$1" | awk '{print $2}' ; }
function extract_ref_from_row() { echo "$1" | awk '{print $3}' ; }
function dir_for_repo() { echo "${DEST}/$1" ; }

function ensure_dir_does_not_exists() {
    if [ -f $1 ] || [ -d $1 ]
    then
        echo "FILE OR DIRECTORY $1 ALREADY EXISTS!" >/dev/stderr
        exit 1
    fi
}

function ensure_root_dir_exists() {
    mkdir -p $DEST
}

function process_row() {
    local row="$1"
    local name="$(extract_name_from_row "$row")"
    local repo="$(extract_repo_from_row "$row")"
    local ref="$(extract_ref_from_row "$row")"
    local dir="$(dir_for_repo "$name")"
    ensure_dir_does_not_exists $dir
    ensure_root_dir_exists
    git clone "$repo" "$dir"
    pushd $dir && git reset --hard "$ref" && popd
}

# For debugging purposes
[ ! -z "$DRYRUN" ] && return

while read row || [ -n "$row" ]
do
    echo -e "PROCESSING:\n${row}\n"
    process_row "$row"
done <<EOF
mylisputils           https://github.com/vitorqb/mylisputils.git           0.5.1
my-show-definitions   https://github.com/vitorqb/my-show-definitions.git   0.0.2
my-fuzzy-cmd-selector https://github.com/vitorqb/my-fuzzy-cmd-selector.git 0.0.3
orgext                https://github.com/vitorqb/orgext.git                0.1.1
lightlispy            https://github.com/vitorqb/lightlispy.git            ffd4b38
EOF
