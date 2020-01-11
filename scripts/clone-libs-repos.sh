#!/bin/bash
USAGE="$0"' [--skip-existing] -- [DESTINATION]
Clones all personal git repositories needed for emacs-init.el from
github to sub folders of `DESTINATION`. Defaults to `~/.emacs.d/other`.

  --skip-existing) 
     Skip a clone if the destination dir already exists, instead of stopping
     with an error.'

# Defaults
DEST=~/.emacs.d/other/
SKIP_EXISTING=0

# Args definition and parsing
SHORT='h'
LONG='skip-existing,help'
OPTS="$(getopt --options "$SHORT" --long "$LONG" --name "$0" -- "$@")"
! [ "$?" = 0 ] && echo "$USAGE" 1>&2 && exit 1
eval set -- "$OPTS"

while [[ "$#" -gt 0 ]]
do
    case "$1" in
        -h|--help)
            echo "$USAGE"
            exit 0
            ;;
        --skip-existing)
            SKIP_EXISTING=1
            shift
            ;;
        --)
            [ ! -z "$2" ] && DEST="$2"
            shift
            shift
            ;;
        *)
            { echo "ERROR: Unkown parameter $1" ; echo "$USAGE" ; } 1>&2
            exit 1
            ;;
    esac
done            

# Helpers
function extract_name_from_row() { echo "$1" | awk '{print $1}' ; }
function extract_repo_from_row() { echo "$1" | awk '{print $2}' ; }
function extract_ref_from_row() { echo "$1" | awk '{print $3}' ; }
function dir_for_repo() { echo "${DEST}/$1" ; }
function ensure_root_dir_exists() { mkdir -p $DEST; }

function should_skip() {
    local dir="$1"
    [[ ("$SKIP_EXISTING" = "1") && (-f "$dir" || -d "$dir") ]]
}

function ensure_dir_does_not_exists() {
    if [ -f $1 ] || [ -d $1 ]
    then
        echo "ERROR: File or directory $1 already exists!" >/dev/stderr
        exit 1
    fi
}

function process_row() {
    local row="$1"
    local name="$(extract_name_from_row "$row")"
    local repo="$(extract_repo_from_row "$row")"
    local ref="$(extract_ref_from_row "$row")"
    local dir="$(dir_for_repo "$name")"
    ensure_root_dir_exists
    [ "$SKIP_EXISTING" = "0" ] && ensure_dir_does_not_exists "$dir"
    if ! should_skip "$dir"
    then
        git clone "$repo" "$dir"
        pushd "$dir" && git reset --hard "$ref" && popd
    else
        echo "WARNING: $dir already exists - skipping..."
    fi
}

# Script
set -e

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
compile-transient     https://github.com/vitorqb/compile-transient.git     0.0.1
EOF
