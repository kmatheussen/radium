#!/usr/bin/env bash
#
# Check the dynamically loaded Pd externals (and optionally the Radium
# executable) for symbol name clashes with libpd and with each other.
#
# Radium loads every external into one process with dlopen(RTLD_GLOBAL), and
# libpd is loaded first. Two classes of clashes are real bugs:
#
#   1. An external defines a *data* symbol that libpd (or the executable)
#      already defines. Without -Wl,-Bsymbolic the external's references bind
#      to libpd's definition and overwrite it: ggee's global
#      "slider_widgetbehavior" used to shadow the widget behavior of Pd's
#      built-in [hsl]/[vsl] classes, so mapping an [hsl] object crashed inside
#      ggee's create_widget(). On macOS pd-lib-builder builds bundles with
#      -flat_namespace and there is no -Bsymbolic equivalent, so there this
#      check (plus making the symbol file-local in the source) is the only
#      protection.
#
#   2. A symbol is undefined in one library, but defined by more than one
#      library in the process. Which definition it binds to then depends on
#      load order. -Bsymbolic does not help here, because the referencing
#      library does not define the symbol itself.
#
# Duplicated symbols between externals, and libpd-vs-external *function*
# duplicates, are reported but do not fail the check: they are mostly shared
# code linked into several binaries and C++ RTTI, and are harmless with
# -Bsymbolic (and because dlsym(handle, ...) searches the handle's own object
# first, which is how the library host calls each library's setup()).
#
# Usage:
#   check_symbol_clashes.sh [options]
#
# Options:
#   --libpd <file>      libpd shared object to check against. Default:
#                       bin/packages/libpd/libs/libpd.so (or .dylib).
#   --externals <dir>   Directory with one subdirectory per external library.
#                       Default: bin/pd/externals.
#   --exe <file>        Extra shared object/executable to include in the
#                       universe, e.g. the Radium binary. Optional.
#   --allow <file>      Allowlist of accepted clash symbols, one symbol per
#                       line, optionally followed by a reason. Lines starting
#                       with # are ignored. Default: symbol_clashes.allow next
#                       to this script.
#   --warn-only         Report everything and always exit 0.
#   --verbose           Also list libpd-vs-external function duplicates and
#                       external-vs-external function duplicates.
#   -h, --help          Show this help.
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

LIBPD=""
EXTERNALS_DIR=""
EXE=""
ALLOW="$SCRIPT_DIR/symbol_clashes.allow"
WARN_ONLY=0
VERBOSE=0

usage() {
    sed -n '3,/^set -euo pipefail/p' "$0" \
        | sed -e '/^set -euo pipefail/d' -e 's/^# \{0,1\}//'
}

while [ $# -gt 0 ]; do
    case "$1" in
        --libpd)
            [ $# -ge 2 ] || { echo "--libpd requires an argument." >&2; exit 2; }
            LIBPD="$2"; shift 2
            ;;
        --externals)
            [ $# -ge 2 ] || { echo "--externals requires an argument." >&2; exit 2; }
            EXTERNALS_DIR="$2"; shift 2
            ;;
        --exe)
            [ $# -ge 2 ] || { echo "--exe requires an argument." >&2; exit 2; }
            EXE="$2"; shift 2
            ;;
        --allow)
            [ $# -ge 2 ] || { echo "--allow requires an argument." >&2; exit 2; }
            ALLOW="$2"; shift 2
            ;;
        --warn-only)
            WARN_ONLY=1; shift
            ;;
        --verbose)
            VERBOSE=1; shift
            ;;
        -h|--help)
            usage; exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            usage >&2
            exit 2
            ;;
    esac
done

if [ -z "$LIBPD" ]; then
    for candidate in \
        "$REPO_ROOT/bin/packages/libpd/libs/libpd.so" \
        "$REPO_ROOT/bin/packages/libpd/libs/libpd.dylib"
    do
        if [ -f "$candidate" ]; then
            LIBPD="$candidate"
            break
        fi
    done
fi
[ -n "$LIBPD" ] || LIBPD="$REPO_ROOT/bin/packages/libpd/libs/libpd.so"
[ -n "$EXTERNALS_DIR" ] || EXTERNALS_DIR="$REPO_ROOT/bin/pd/externals"

if [ ! -f "$LIBPD" ]; then
    echo "libpd not found: $LIBPD" >&2
    echo "Build it first, for example: cd bin/packages && ./build.sh" >&2
    exit 2
fi
if [ ! -d "$EXTERNALS_DIR" ]; then
    echo "externals directory not found: $EXTERNALS_DIR" >&2
    exit 2
fi
if [ -n "$EXE" ] && [ ! -f "$EXE" ]; then
    echo "executable not found: $EXE" >&2
    exit 2
fi

DARWIN=0
NM_MODE=(-D)
case "$(uname -s)" in
    Darwin)
        DARWIN=1
        NM_MODE=(-g)
        ;;
esac

FILES=("$LIBPD")
while IFS= read -r -d '' file; do
    FILES+=("$file")
done < <(find "$EXTERNALS_DIR" -mindepth 2 -maxdepth 2 -type f \
    \( -name '*.pd_linux' -o -name '*.pd_darwin' \
       -o -name '*.so' -o -name '*.dylib' \) -print0)
if [ -n "$EXE" ]; then
    FILES+=("$EXE")
fi

{
    for file in "${FILES[@]}"; do
        printf 'FILE\t%s\n' "$file"
        nm "${NM_MODE[@]}" "$file" 2>/dev/null || true
    done
} | awk \
    -v libpd="$LIBPD" \
    -v extdir="$EXTERNALS_DIR/" \
    -v allow="$ALLOW" \
    -v darwin="$DARWIN" \
    -v warnonly="$WARN_ONLY" \
    -v verbose="$VERBOSE" '
function short(f,    p) {
    if (f == libpd)
        return "libpd"
    p = index(f, extdir)
    if (p == 1)
        return substr(f, length(extdir) + 1)
    return f
}

BEGIN {
    nfiles = 0
    ndef = 0
    nundef = 0
    nlibfun = 0
    nextdata = 0
    nextfun = 0
    nknown = 0
    nfail = 0

    if (allow != "") {
        while ((getline line < allow) > 0) {
            sub(/#.*/, "", line)
            gsub(/^[ \t]+/, "", line)
            gsub(/[ \t]+$/, "", line)
            if (line == "")
                continue
            split(line, fields, /[ \t]+/)
            allowed[fields[1]] = 1
        }
        close(allow)
    }
}

$1 == "FILE" {
    i = index($0, "\t")
    cur = substr($0, i + 1)
    nfiles++
    next
}

{
    t = ""
    name = ""
    for (i = 1; i <= NF; i++) {
        if ($i ~ /^[A-Za-z]$/) {
            t = $i
            name = $(i + 1)
            break
        }
    }
    if (t == "" || name == "")
        next
    sub(/@.*$/, "", name)
    if (darwin)
        sub(/^_/, "", name)
    if (name == "")
        next

    if (t == "U") {
        nundef++
        uref[name] = uref[name] "\t" cur
        next
    }

    ndef++
    def[name, ++ndefcount[name]] = cur
    dtype[name, ndefcount[name]] = t
}

END {
    # Duplicated definitions.
    for (sym in ndefcount) {
        if (ndefcount[sym] < 2)
            continue

        haslib = 0
        isdata = 0
        strongdata = 0
        allweak = 1
        providers = ""
        for (i = 1; i <= ndefcount[sym]; i++) {
            f = def[sym, i]
            t = dtype[sym, i]
            providers = providers (providers != "" ? ", " : "") short(f) " (" t ")"
            if (f == libpd)
                haslib = 1
            if (t ~ /^[BDRGSCVbdrgscv]$/) {
                isdata = 1
                if (t ~ /^[BDRGSCV]$/)
                    strongdata = 1
            }
            if (t !~ /^[WwVv]$/)
                allweak = 0
        }

        if (haslib && isdata && strongdata && !allweak) {
            if (sym in allowed) {
                nknown++
            } else {
                fail[nfail++] = "external data symbol shadows libpd: " \
                    sym " -- " providers
            }
        } else if (haslib) {
            nlibfun++
            if (verbose)
                report[nreport++] = "libpd/external function duplicate: " \
                    sym " -- " providers
        } else if (isdata) {
            nextdata++
            datadup[nextdata] = sym " -- " providers
        } else {
            nextfun++
            if (verbose)
                report[nreport++] = "external/external function duplicate: " \
                    sym " -- " providers
        }
    }

    # Ambiguous undefined references: undefined here, defined by >= 2 objects.
    for (sym in uref) {
        if (ndefcount[sym] + 0 < 2)
            continue
        providers = ""
        for (i = 1; i <= ndefcount[sym]; i++)
            providers = providers (providers != "" ? ", " : "") short(def[sym, i])
        refs = ""
        nrefs = split(uref[sym], r, "\t")
        for (i = 1; i <= nrefs; i++) {
            if (r[i] == "")
                continue
            if (index(refs, short(r[i])) > 0)
                continue
            refs = refs (refs != "" ? ", " : "") short(r[i])
        }
        fail[nfail++] = "ambiguous undefined reference: " sym \
            " (defined by " providers "; referenced by " refs ")"
    }

    printf("Symbol clash check: %d shared objects, %d defined symbols, " \
        "%d undefined symbols\n", nfiles, ndef, nundef)

    if (nknown > 0)
        printf("Allowlisted libpd data clashes: %d\n", nknown)
    if (nlibfun > 0)
        printf("Report: %d libpd-vs-external function duplicate(s)%s\n", \
            nlibfun, verbose ? "" : " (use --verbose to list)")
    if (nextdata > 0) {
        printf("Report: %d external-vs-external data duplicate(s):\n", nextdata)
        for (i = 1; i <= nextdata; i++)
            printf("  %s\n", datadup[i])
    }
    if (nextfun > 0)
        printf("Report: %d external-vs-external function duplicate(s)%s\n", \
            nextfun, verbose ? "" : " (use --verbose to list)")

    for (i = 0; i < nreport; i++)
        printf("  report: %s\n", report[i])

    if (nfail > 0) {
        printf("\n")
        for (i = 0; i < nfail; i++)
            printf("FAIL: %s\n", fail[i])
        if (warnonly)
            printf("Symbol clash check: %d problem(s) (warning only)\n", nfail)
        else
            printf("Symbol clash check FAILED: %d problem(s)\n", nfail)
    } else {
        printf("Symbol clash check OK\n")
    }

    exit (nfail > 0 && !warnonly) ? 1 : 0
}
'
