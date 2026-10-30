#!/usr/bin/env bash
# Regenerate the numbers, tables and figures of the book from the notebooks.
# Only the notebooks listed in book_outputs/index.csv are run; outputs go to book_outputs/chNN/.
# See README.md.

set -euo pipefail

usage() {
  cat <<'EOF'
Usage: ./make_book_outputs.sh [options] [CHAPTER ...]

  CHAPTER      chapter numbers to build, e.g. "2 4" (default: all chapters in book_outputs/index.csv)

Options:
  --list       only show which notebooks would run
  --missing    only run notebooks with at least one output file missing
  --setup      install the R and Python packages the selected notebooks need, then exit
  -j N         run up to N notebooks in parallel (default 1)
  -h, --help   show this help

Environment:
  RSCRIPT      R interpreter for .irnb notebooks (default: Rscript)
  IPYTHON      IPython for .ipynb notebooks (default: ipython)
  PYTHON       Python for the helper script and pip (default: python3)

Notebooks run as scripts (like the repo's CI), in a temporary directory, without their install commands.
Logs: book_outputs/logs/<notebook>.log
EOF
}

ROOT="$(cd "$(dirname "$0")" && pwd)"
OUT="$ROOT/book_outputs"
HELPER="$ROOT/book_outputs/helper.py"
RSCRIPT="${RSCRIPT:-Rscript}"
IPYTHON="${IPYTHON:-ipython}"
PYTHON="${PYTHON:-python3}"
export ROOT OUT HELPER RSCRIPT IPYTHON PYTHON

# Run a single notebook (called in parallel through xargs)
run_one() {
  local nb="$1" name work script start status secs
  name="$(basename "${nb%.*}")"
  work="$(mktemp -d "${TMPDIR:-/tmp}/book_outputs.XXXXXX")"
  mkdir -p "$OUT/logs"
  start=$(date +%s)
  status=0
  case "$nb" in
    *.irnb) script="$work/$name.R"
            "$PYTHON" "$HELPER" script "$nb" "$script"
            (cd "$work" && BOOK_OUTPUT_DIR="$OUT" "$RSCRIPT" "$script") > "$OUT/logs/$name.log" 2>&1 || status=$? ;;
    *.ipynb) script="$work/$name.ipy"
             "$PYTHON" "$HELPER" script "$nb" "$script"
             (cd "$work" && BOOK_OUTPUT_DIR="$OUT" MPLBACKEND=Agg "$IPYTHON" "$script") \
               > "$OUT/logs/$name.log" 2>&1 || status=$? ;;
    *) echo "unknown notebook type: $nb" >&2; return 1 ;;
  esac
  secs=$(( $(date +%s) - start ))
  rm -rf "$work"
  if [ "$status" -eq 0 ]; then
    printf '  ok      %-55s %3dm%02ds\n' "$nb" $((secs / 60)) $((secs % 60))
  else
    printf '  FAILED  %-55s %3dm%02ds  (see book_outputs/logs/%s.log)\n' "$nb" $((secs / 60)) $((secs % 60)) "$name"
    return 1
  fi
}

if [ "${1:-}" = "--run-one" ]; then
  run_one "$2"
  exit $?
fi

jobs=1; setup=0; list=0; missing=""
chapters=()
while [ $# -gt 0 ]; do
  case "$1" in
    -h|--help) usage; exit 0 ;;
    --list) list=1 ;;
    --missing) missing="--missing" ;;
    --setup) setup=1 ;;
    -j) jobs="$2"; shift ;;
    -j*) jobs="${1#-j}" ;;
    [0-9]*) chapters+=("$1") ;;
    *) echo "unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

cd "$ROOT"
notebooks=()
while IFS= read -r nb; do
  [ -n "$nb" ] && notebooks+=("$nb")
done < <("$PYTHON" "$HELPER" notebooks $missing ${chapters[@]+"${chapters[@]}"})

if [ ${#notebooks[@]} -eq 0 ]; then
  echo "Nothing to run."
  exit 0
fi

if [ "$list" -eq 1 ]; then
  printf '%s\n' "${notebooks[@]}"
  exit 0
fi

if [ "$setup" -eq 1 ]; then
  r_setup="$(mktemp "${TMPDIR:-/tmp}/book_outputs_setup.XXXXXX")"
  echo 'options(repos = c(CRAN = "https://cloud.r-project.org"))' > "$r_setup"
  pip_pkgs=()
  while IFS= read -r line; do
    case "$line" in
      "R "*) echo "${line#R }" >> "$r_setup" ;;
      "PIP "*) pip_pkgs+=("${line#PIP }") ;;
    esac
  done < <("$PYTHON" "$HELPER" installs "${notebooks[@]}")
  echo "Installing R packages:"; sed 1d "$r_setup" | sed 's/^/  /'
  "$RSCRIPT" "$r_setup"
  rm -f "$r_setup"
  for pkgs in ${pip_pkgs[@]+"${pip_pkgs[@]}"}; do
    echo "pip install $pkgs"
    "$PYTHON" -m pip install $pkgs
  done
  exit 0
fi

echo "Running ${#notebooks[@]} notebook(s) with $jobs parallel job(s); outputs in book_outputs/"
failed=0
printf '%s\n' "${notebooks[@]}" | xargs -P "$jobs" -I{} "$ROOT/$(basename "$0")" --run-one {} || failed=1

absent=()
while IFS= read -r f; do
  [ -e "$OUT/$f" ] || absent+=("$f")
done < <("$PYTHON" "$HELPER" expected ${chapters[@]+"${chapters[@]}"})

if [ ${#absent[@]} -gt 0 ]; then
  echo "Missing outputs:"
  printf '  book_outputs/%s\n' "${absent[@]}"
  failed=1
fi
if [ "$failed" -eq 0 ]; then
  echo "All outputs written."
fi
exit "$failed"
