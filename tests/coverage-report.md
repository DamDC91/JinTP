# GNATcoverage

Run the complete test suite with GNATcoverage from this directory:

```sh
make coverage
```

The test crate declares `gnatcov_bin`, and the Make target invokes the script
inside `alr exec`, which supplies the GNAT toolchain and dependency environment.
The script prepares the GNATcoverage runtime for the selected toolchain the first
time, instruments only JinTP library units (`Jintp*`), rebuilds the tests, runs
them, and creates a statement coverage report.

Generated files are kept under `coverage-output/`:

- `summary.txt`: raw GNATcoverage report, including line and statement totals.
- `files-summary.md`: a compact table with line and statement coverage percentages for each file.
- `report/*.xcov`: annotated source, where `+` marks covered lines and `-`
  marks uncovered lines.
- `jintp-tests.srctrace`: execution data collected when the test program exits.
- `gnatcov-rts/`: the GNATcoverage runtime built for the active GNAT toolchain.

The installed GNATcoverage binary supports text and `.xcov` output. HTML output
is not enabled in this build.
