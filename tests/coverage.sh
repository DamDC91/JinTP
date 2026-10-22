#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

MODE="${1:-stmt}"
case "$MODE" in
    stmt)
        GNATCOV_LEVEL=stmt
        ;;
    branch)
        # GNATcoverage's source-level branch analogue is decision coverage.
        GNATCOV_LEVEL=stmt+decision
        ;;
    *)
        echo "Usage: $0 [stmt|branch]" >&2
        exit 2
        ;;
esac

BASE_OUTPUT_DIR="$PWD/coverage-output"
OUTPUT_DIR="$BASE_OUTPUT_DIR/$MODE"
RUNTIME_PREFIX="$BASE_OUTPUT_DIR/gnatcov-rts"
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
    --level="$GNATCOV_LEVEL" \
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
    --level="$GNATCOV_LEVEL" \
    --annotate=report,xcov \
    --output-dir="$REPORT_DIR" \
    "$TRACE_FILE" \
    -P jintp_tests.gpr \
    --projects=../jintp.gpr \
    --units='Jintp*' \
    > "$OUTPUT_DIR/summary.txt"

{
    if [[ "$MODE" == "branch" ]]; then
        printf '| File | Line coverage | Statement coverage | Decision coverage |\n'
        printf '| --- | ---: | ---: | ---: |\n'
    else
        printf '| File | Line coverage | Statement coverage |\n'
        printf '| --- | ---: | ---: |\n'
    fi
    for annotation in "$REPORT_DIR"/*.xcov; do
        if [[ "$MODE" == "branch" ]]; then
            awk '
            NR == 1 {
                file = $0
                sub(/^.*\//, "", file)
                sub(/:$/, "", file)
            }
            / lines covered$/ { lines = $0 }
            / statement coverage/ { statements = $0 }
            / decision coverage/ { decisions = $0 }
            END {
                if (lines == "") lines = "no code"
                if (statements == "") statements = "no code"
                if (decisions == "") decisions = "no code"
                printf "| `%s` | %s | %s | %s |\n", file, lines, statements, decisions
            }
            ' "$annotation"
        else
            awk '
            NR == 1 {
                file = $0
                sub(/^.*\//, "", file)
                sub(/:$/, "", file)
            }
            / lines covered$/ { lines = $0 }
            / statement coverage/ { statements = $0 }
            END {
                if (lines == "") lines = "no code"
                if (statements == "") statements = "no code"
                printf "| `%s` | %s | %s |\n", file, lines, statements
            }
            ' "$annotation"
        fi
    done
} > "$OUTPUT_DIR/files-summary.md"

echo "GNATcoverage summary: $OUTPUT_DIR/summary.txt"
echo "Per-file percentages: $OUTPUT_DIR/files-summary.md"
echo "Annotated source files: $REPORT_DIR/"
echo "Raw execution trace: $TRACE_FILE"
