# GNATcoverage

Run statement coverage with GNATcoverage from this directory:

```sh
make coverage
```

Run statement and branch decision coverage with:

```sh
make coverage-branch
```

The script also accepts `stmt` or `branch` as its first argument. GNATcoverage
expresses source-level branch coverage as decision coverage, so the `branch`
mode uses `--level=stmt+decision`: it reports statement coverage and whether
each decision has taken both its true and false outcomes.

The test crate declares `gnatcov_bin`, and the Make target invokes the script
inside `alr exec`, which supplies the GNAT toolchain and dependency environment.
The script prepares the GNATcoverage runtime for the selected toolchain the first
time, instruments only JinTP library units (`Jintp*`), rebuilds the tests, runs
them, and creates a statement coverage report.

Generated files are kept under a mode-specific directory in `coverage-output/`
(`stmt/` or `branch/`):

- `summary.txt`: raw GNATcoverage report, including line and statement totals;
  branch mode also includes decision results.
- `files-summary.md`: a compact table with line and statement coverage percentages for each file.
- In branch mode, `files-summary.md` also includes decision coverage.
- `report/*.xcov`: annotated source, where `+` marks covered lines and `-`
  marks uncovered lines.
- `jintp-tests.srctrace`: execution data collected when the test program exits.

The shared GNATcoverage runtime is kept at `coverage-output/gnatcov-rts/` and
built for the active GNAT toolchain.

The installed GNATcoverage binary supports text and `.xcov` output. HTML output
is not enabled in this build.
