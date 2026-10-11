#!/usr/bin/env bash
#
# Update the pinned Pd external sources in pd_externals.list to their newest
# upstream version, download the new tarballs, and update the manifest
# (tarball name, source directory and ref) in place.
#
# Usage:
#   update_pd_externals.sh [options] <name>...|all
#
# Options:
#   --check         Only report which libraries have newer versions; do not
#                   download or change anything. Exit status is 0 when all
#                   selected libraries are up to date and 1 when updates are
#                   available (2 on error).
#   --dry-run       Print what would be done, without downloading or writing.
#   --force         Update even when the pinned and upstream refs are equal.
#   -h, --help      Show this help.
#
# check_updated_pd_externals.sh is a thin wrapper for "--check".
#
# The newest ref is found with "git ls-remote":
#   - ref_type = tag:    newest tag matching tag_glob, sorted by version.
#   - ref_type = branch: the current tip of branch.
# For libraries with a submodule (currently mrpeach), the submodule is handled
# the same way using the submodule_* manifest keys.
#
set -euo pipefail

# Resolve symlinks, so the script works both from
# pd_externals/update_pd_externals.sh and through the
# bin/packages/update_pd_externals.sh convenience symlink.
SOURCE="${BASH_SOURCE[0]}"
while [ -L "$SOURCE" ]; do
    DIR="$(cd -P "$(dirname "$SOURCE")" && pwd)"
    SOURCE="$(readlink "$SOURCE")"
    case "$SOURCE" in
        /*) ;;
        *) SOURCE="$DIR/$SOURCE" ;;
    esac
done
SCRIPT_DIR="$(cd -P "$(dirname "$SOURCE")" && pwd)"
MANIFEST="$SCRIPT_DIR/pd_externals.list"

CHECK_ONLY=false
DRY_RUN=false
FORCE=false
SELECTED=()

usage() {
    sed -n '2,/^set -euo pipefail/p' "$0" | sed -e '/^set -euo pipefail/d' -e 's/^# \{0,1\}//'
}

while [ $# -gt 0 ]; do
    case "$1" in
        --check) CHECK_ONLY=true; shift ;;
        --dry-run) DRY_RUN=true; shift ;;
        --force) FORCE=true; shift ;;
        -h|--help) usage; exit 0 ;;
        --) shift; break ;;
        -*) echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
        *) break ;;
    esac
done

[ $# -gt 0 ] || { echo "No library names given (use \"all\" for every library)." >&2; exit 2; }

while [ $# -gt 0 ]; do
    SELECTED+=("$1")
    shift
done

[ -f "$MANIFEST" ] || { echo "Manifest not found: $MANIFEST" >&2; exit 2; }


trim() {
    echo "$1" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'
}

# Sets key = value inside [section]. The key must already exist.
manifest_set() {
    local section="$1" key="$2" value="$3"
    local tmp="$MANIFEST.tmp.$$"
    awk -v section="$section" -v key="$key" -v value="$value" '
        /^\[/ { sec = substr($0, 2, length($0)-2) }
        sec == section && $0 ~ "^[ \t]*" key "[ \t]*=" {
            print key " = " value
            next
        }
        { print }
    ' "$MANIFEST" > "$tmp"
    mv "$tmp" "$MANIFEST"
}

latest_tag() {
    local upstream="$1" glob="$2"
    local tags tag
    tags="$(git ls-remote --sort=-version:refname --tags "$upstream" 2>/dev/null \
        | grep -v '\^{}' | sed 's|.*refs/tags/||')" || return 1
    while IFS= read -r tag; do
        [ -n "$tag" ] || continue
        case "$tag" in
            $glob) printf '%s\n' "$tag"; return 0 ;;
        esac
    done <<< "$tags"
    return 1
}

branch_tip() {
    local upstream="$1" branch="$2"
    git ls-remote "$upstream" "refs/heads/$branch" 2>/dev/null | awk '{print $1}'
}

# Outputs: SRC_LATEST
determine_latest() {
    local upstream="$1" ref_type="$2" tag_glob="$3" branch="$4"
    if [ "$ref_type" = "branch" ]; then
        branch_tip "$upstream" "$branch"
    else
        latest_tag "$upstream" "${tag_glob:-*}"
    fi
}

sanitize_ref() {
    echo "$1" | sed 's|/|-|g'
}

download() {
    local url="$1" dest="$2"
    if $DRY_RUN; then
        echo "+ curl -fL -o $dest $url"
        return 0
    fi
    echo "  downloading $url"
    rm -f "$dest.tmp"
    curl -fL --retry 2 --max-time 600 -o "$dest.tmp" "$url" || {
        rm -f "$dest.tmp"
        return 1
    }
    mv "$dest.tmp" "$dest"
}

# Removes the given paths from the (single-top-level-directory) tarball and
# rewrites it in place. Keeps vendored source archives small when part of the
# upstream tree is not used by Radium.
prune_tarball() {
    local tarball="$1"
    shift
    [ -f "$tarball" ] || return 1

    local tmpdir
    tmpdir="$(mktemp -d)"
    if ! tar xzf "$tarball" -C "$tmpdir"; then
        rm -rf "$tmpdir"
        return 1
    fi

    local top
    top="$(find "$tmpdir" -mindepth 1 -maxdepth 1 -type d | head -n 1)"
    if [ -z "$top" ]; then
        rm -rf "$tmpdir"
        return 1
    fi

    ( cd "$top" && rm -rf "$@" )
    ( cd "$tmpdir" && tar czf "$tarball.new" "$(basename "$top")" )
    mv "$tarball.new" "$tarball"
    rm -rf "$tmpdir"
    return 0
}

# Reports (and unless --check/--dry-run, downloads) one source.
# Inputs:  ROLE SOURCE_NAME UPSTREAM REF_TYPE TAG_GLOB BRANCH REF URL_TMPL OLD_TARBALL
# Outputs: SRC_LATEST SRC_TARBALL SRC_SRCDIR
process_source() {
    local role="$1" source_name="$2" upstream="$3" ref_type="$4" tag_glob="$5"
    local branch="$6" ref="$7" url_tmpl="$8"
    local lib_name="${9:-$2}"

    SRC_LATEST=""
    SRC_TARBALL=""
    SRC_SRCDIR=""

    local latest
    latest="$(determine_latest "$upstream" "$ref_type" "$tag_glob" "$branch")" || true
    if [ -z "$latest" ]; then
        echo "$source_name: unable to determine the latest $role ref from $upstream" >&2
        return 1
    fi
    SRC_LATEST="$latest"

    if [ "$latest" = "$ref" ] && ! $FORCE; then
        printf '%-12s %-9s up to date (%s)\n' "$source_name" "$role" "$ref"
        return 0
    fi

    printf '%-12s %-9s %s -> %s\n' "$source_name" "$role" "$ref" "$latest"

    if $CHECK_ONLY; then
        UPDATES_AVAILABLE=true
        return 0
    fi

    local short
    short="$(sanitize_ref "$latest")"
    if [ "$ref_type" = "branch" ]; then
        short="$(echo "$short" | cut -c1-12)"
    fi
    SRC_TARBALL="$lib_name-$short.tar.gz"

    local dest="$SCRIPT_DIR/$SRC_TARBALL"
    if $DRY_RUN; then
        echo "+ would download $(printf '%s' "${url_tmpl//%ref%/$latest}")"
        echo "+ would update manifest: tarball=$SRC_TARBALL ref=$latest"
        SRC_SRCDIR="(dry run)"
        return 0
    fi

    if [ ! -f "$dest" ] || $FORCE; then
        local url="${url_tmpl//%ref%/$latest}"
        download "$url" "$dest" || {
            echo "$source_name: download failed: $url" >&2
            return 1
        }
    fi

    SRC_SRCDIR="$(tar tzf "$dest" 2>/dev/null | head -n 1 | sed 's|/$||')"
    if [ -z "$SRC_SRCDIR" ]; then
        echo "$source_name: $dest is not a valid tarball" >&2
        return 1
    fi

    return 0
}

UPDATES_AVAILABLE=false
FAILED=false

matches_selection() {
    local name="$1" selected
    for selected in "${SELECTED[@]}"; do
        [ "$selected" = "all" ] && return 0
        [ "$selected" = "$name" ] && return 0
    done
    return 1
}

process_section() {
    [ -n "$section" ] || return 0
    matches_selection "$section" || return 0

    echo
    echo "==== $section ===="

    if process_source "library" "$section" "$upstream" "$ref_type" "$tag_glob" \
        "$branch" "$ref" "$url" "$section"; then
        if [ -n "$SRC_TARBALL" ] && ! $CHECK_ONLY && ! $DRY_RUN; then
            if [ -n "$exclude" ]; then
                # shellcheck disable=SC2086
                prune_tarball "$SCRIPT_DIR/$SRC_TARBALL" $exclude
            fi
            manifest_set "$section" tarball "$SRC_TARBALL"
            manifest_set "$section" srcdir "$SRC_SRCDIR"
            manifest_set "$section" ref "$SRC_LATEST"
        fi
    else
        FAILED=true
    fi

    if [ -n "$submodule_path" ]; then
        if process_source "submodule" "$section" "$submodule_upstream" \
            "$submodule_ref_type" "$submodule_tag_glob" "$submodule_branch" \
            "$submodule_ref" "$submodule_url" "$submodule_path"; then
            if [ -n "$SRC_TARBALL" ] && ! $CHECK_ONLY; then
                if ! $DRY_RUN; then
                    manifest_set "$section" submodule_tarball "$SRC_TARBALL"
                    manifest_set "$section" submodule_ref "$SRC_LATEST"
                fi
            fi
        else
            FAILED=true
        fi
    fi
}

section=""
tarball=""
srcdir=""
exclude=""
upstream=""
ref_type=""
tag_glob=""
branch=""
ref=""
url=""
submodule_path=""
submodule_tarball=""
submodule_upstream=""
submodule_ref_type=""
submodule_tag_glob=""
submodule_branch=""
submodule_ref=""
submodule_url=""

while IFS= read -r line || [ -n "$line" ]; do
    line="${line%$'\r'}"
    case "$line" in ""|\#*) continue ;; esac
    case "$line" in
        \[*\])
            process_section
            section="${line#[}"; section="${section%]}"
            tarball=""; srcdir=""; exclude=""; upstream=""; ref_type=""; tag_glob=""
            branch=""; ref=""; url=""
            submodule_path=""; submodule_tarball=""; submodule_upstream=""
            submodule_ref_type=""; submodule_tag_glob=""; submodule_branch=""
            submodule_ref=""; submodule_url=""
            ;;
        *=*)
            key="$(trim "${line%%=*}")"
            value="$(trim "${line#*=}")"
            case "$key" in
                tarball) tarball="$value" ;;
                srcdir) srcdir="$value" ;;
                exclude) exclude="$value" ;;
                upstream) upstream="$value" ;;
                ref_type) ref_type="$value" ;;
                tag_glob) tag_glob="$value" ;;
                branch) branch="$value" ;;
                ref) ref="$value" ;;
                url) url="$value" ;;
                submodule_path) submodule_path="$value" ;;
                submodule_tarball) submodule_tarball="$value" ;;
                submodule_upstream) submodule_upstream="$value" ;;
                submodule_ref_type) submodule_ref_type="$value" ;;
                submodule_tag_glob) submodule_tag_glob="$value" ;;
                submodule_branch) submodule_branch="$value" ;;
                submodule_ref) submodule_ref="$value" ;;
                submodule_url) submodule_url="$value" ;;
            esac
            ;;
    esac
done < "$MANIFEST"
process_section

echo
if $FAILED; then
    echo "Some sources could not be checked/updated." >&2
    exit 2
fi

if $CHECK_ONLY; then
    if $UPDATES_AVAILABLE; then
        echo "Updates are available."
        exit 1
    fi
    echo "Everything is up to date."
    exit 0
fi

echo "Done. Remember to rebuild:"
echo "    cd \"$SCRIPT_DIR/..\" && ./build.sh"
