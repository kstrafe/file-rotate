# Changelog

## 0.9.0 - Unreleased

### Breaking changes
- Replaced the `chrono` dependency with `jiff`. `FileLimit::Age` now takes a
  `std::time::Duration` instead of `chrono::Duration`.
- Minimum supported Rust version is now 1.85 (required by `jiff` 1.0).

### Changed
- Timestamp `format` strings are now interpreted by `jiff`'s `strftime`/`strptime`.
  The default format is unaffected, but exotic custom format specifiers may behave
  differently than with `chrono`.
- Migrated to Rust edition 2024.
- Updated `flate2` to 1.1.

### Fixed
- A very large `FileLimit::Age` (e.g. `Duration::MAX`) no longer panics.
- Compressing a file now truncates an existing `.gz` destination instead of leaving
  trailing data from an older file.

## 0.8.0

### Breaking changes
- `FileRotate::new` takes an `Option<OpenOptions>` argument on all platforms,
  replacing the Unix-only `mode: Option<u32>` argument.
- Added `FileLimit::Unlimited`.
