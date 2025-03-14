#!/bin/bash
USAGE="$0"' [--update-existing] [--skip-existing] -- [DESTINATION]
Clones all personal git repositories needed for emacs-init.el from
github to sub folders of `DESTINATION`. Defaults to `~/.emacs.d/emacs_init_deps`.

  --skip-existing) 
     Skip a clone if the destination dir already exists, instead of stopping
     with an error. Use this if you just want to download a new dependency.

  --update-existing)
     If a destination already exists, assumes it has the correct git repository,
     updates it and resets HEAD to the ref. Use this if you are updating.'

# Defaults
DEST=~/.emacs.d/emacs_init_deps/
SKIP_EXISTING=0
UPDATE_EXISTING=0

# Args definition and parsing
SHORT='h'
LONG='skip-existing,update-existing,help'
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
        --update-existing)
            UPDATE_EXISTING=1
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

# Sanity check
if [ "$SKIP_EXISTING" = "1" ] && [ "$UPDATE_EXISTING" = "1" ]
then
    { echo "ERROR: Can not skip and update existing at the same time" ; echo "$USAGE" ; } 1>&2
    exit 1
fi

# Helpers
function extract_name_from_row() { echo "$1" | awk '{print $1}' ; }
function extract_repo_from_row() { echo "$1" | awk '{print $2}' ; }
function extract_ref_from_row() { echo "$1" | awk '{print $3}' ; }
function dir_for_repo() { echo "${DEST}/$1" ; }
function ensure_root_dir_exists() { mkdir -p $DEST; }

function should_skip_dir() {
    local dir="$1"
    [[ ("$SKIP_EXISTING" = "1") && (-f "$dir" || -d "$dir") ]]
}

function should_update_dir() {
    local dir="$1"
    [[ ("$UPDATE_EXISTING" = "1") && (-f "$dir" || -d "$dir") ]]
}

function ensure_dir_does_not_exists() {
    if [ -f $1 ] || [ -d $1 ]
    then
        echo "ERROR: File or directory $1 already exists!" >/dev/stderr
        exit 1
    fi
}

function update_dir() {
    local dir="$1"
    local ref="$2"
    pushd "$dir" && git remote update && git reset --hard "$ref" && popd
}

function git_clone_dir() {
    local dir="$1"
    local repo="$2"
    local ref="$3"
    ensure_dir_does_not_exists "$dir"
    git clone "$repo" "$dir"
    pushd "$dir" && git reset --hard "$ref" && popd
}

function process_row() {
    local row="$1"
    local name="$(extract_name_from_row "$row")"
    local repo="$(extract_repo_from_row "$row")"
    local ref="$(extract_ref_from_row "$row")"
    local dir="$(dir_for_repo "$name")"

    ensure_root_dir_exists

    if ! should_skip_dir "$dir"
    then
        if should_update_dir "$dir"
        then
            update_dir "$dir" "$ref"
        else
            git_clone_dir "$dir" "$repo" "$ref"
        fi
    else
        echo "WARNING: $dir already exists - skipping..."
    fi
}

# Script
set -e

while read row || [ -n "$row" ]
do
    echo -e "\nPROCESSING:\n${row}\n"
    process_row "$row"
done <<EOF
my-fuzzy-cmd-selector https://github.com/vitorqb/my-fuzzy-cmd-selector.git 0.0.3
orgext                https://github.com/vitorqb/orgext.git                0.3.0
lightlispy            https://github.com/vitorqb/lightlispy.git            ffd4b38
ag                    https://github.com/vitorqb/ag.el.git                 98a383a
EOF
