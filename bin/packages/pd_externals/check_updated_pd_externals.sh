#!/usr/bin/env bash
#
# Report which pinned Pd external sources in pd_externals.list have a newer
# upstream version available. A thin wrapper around:
#
#     update_pd_externals.sh --check <name>...|all
#
# Exit status: 0 = everything up to date, 1 = updates available, 2 = error.
#
set -euo pipefail

# Resolve symlinks, so the script works both from
# pd_externals/check_updated_pd_externals.sh and through the
# bin/packages/check_updated_pd_externals.sh convenience symlink.
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

exec "$SCRIPT_DIR/update_pd_externals.sh" --check "$@"
