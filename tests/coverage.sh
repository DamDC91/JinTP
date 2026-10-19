#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

OUTPUT_DIR="$PWD/coverage-output"
RUNTIME_PREFIX="$OUTPUT_DIR/gnatcov-rts"
TRACE_FILE="$OUTPUT_DIR/jintp-tests.srctrace"
REPORT_DIR="$OUTPUT_DIR/report"
RUNTIME_PROJECT="$RUNTIME_PREFIX/share/gpr"

mkdir -p "$OUTPUT_DIR"
if [[ ! -f "$RUNTIME_PROJECT/gnatcov_rts.gpr" ]]; then
    gnatcov setup --prefix="$RUNTIME_PREFIX"
fi

export GPR_PROJECT_PATH="$RUNTIME_PROJECT${GPR_PROJECT_PATH:+:$GPR_PROJECT_PATH}"

gnatcov instrument \
    -P jintp_tests.gpr \
    --projects=../jintp.gpr \
    --units='Jintp*' \
    --level=stmt \
    --dump-trigger=main-end \
    --dump-filename-env-var=JINTP_GNATCOV_TRACE \
    --relocate-build-tree

gprbuild -P jintp_tests.gpr \
    --src-subdirs=gnatcov-instr \
    --implicit-with=gnatcov_rts.gpr \
    -j0

JINTP_GNATCOV_TRACE="$TRACE_FILE" ./bin/jintp_tests

mkdir -p "$REPORT_DIR"
gnatcov coverage \
    --level=stmt \
    --annotate=report,xcov \
    --output-dir="$REPORT_DIR" \
    "$TRACE_FILE" \
    -P jintp_tests.gpr \
    --projects=../jintp.gpr \
    --units='Jintp*' \
    > "$OUTPUT_DIR/summary.txt"

{
    printf '| File | Line coverage | Statement coverage |\n'
    printf '| --- | ---: | ---: |\n'
    for annotation in "$REPORT_DIR"/*.xcov; do
        awk '
            NR == 1 {
                file = $0
                sub(/^.*\//, "", file)
                sub(/:$/, "", file)
            }
            NR == 2 { lines = $0 }
            NR == 3 { statements = $0 }
            END { printf "| `%s` | %s | %s |\n", file, lines, statements }
        ' "$annotation"
    done
} > "$OUTPUT_DIR/files-summary.md"

echo "GNATcoverage summary: $OUTPUT_DIR/summary.txt"
echo "Per-file percentages: $OUTPUT_DIR/files-summary.md"
echo "Annotated source files: $REPORT_DIR/"
echo "Raw execution trace: $TRACE_FILE"
