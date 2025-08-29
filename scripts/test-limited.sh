#!/usr/bin/env bash
set -euo pipefail

# Move to repo root (script resides in scripts/)
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
REPO_ROOT="$(cd -- "${SCRIPT_DIR}/.." >/dev/null 2>&1 && pwd)"
cd "$REPO_ROOT"

# Configurable memory limit (default 6G); override with MEM_LIMIT env var
MEM_LIMIT="${MEM_LIMIT:-6G}"

# Concurrency and compile settings (override via env if desired)
RUST_TEST_THREADS="${RUST_TEST_THREADS:-1}"
RUSTFLAGS="${RUSTFLAGS:--Ccodegen-units=1}"
CARGO_JOBS="${CARGO_JOBS:-1}"

# Run tests under a systemd user scope with a hard memory cap.
# Removes -q, adds --nocapture, and forces single-threaded tests to reduce memory.
exec systemd-run --user --scope \
  -p "MemoryMax=${MEM_LIMIT}" \
  -p "MemorySwapMax=0" \
  -p "OOMPolicy=kill" \
  nix develop --command -- env \
    RUST_TEST_THREADS="${RUST_TEST_THREADS}" \
    RUSTFLAGS="${RUSTFLAGS}" \
    cargo test --jobs "${CARGO_JOBS}" -- --nocapture --test-threads="${RUST_TEST_THREADS}" "$@"
