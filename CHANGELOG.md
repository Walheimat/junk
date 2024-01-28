# Changelog

## [v0.2.0]

### Added

- Usage section to README.
- `junk-setup-use-package` to make sure ensuring does not install
  `junk` pack packages.
- Helper `junk--read-package` now only returns packs that contain
  packages, recipes or extras that haven't been installed.
- `marginalia` annotation now indicates which packages are already
  installed.

### Changed

- Switched to using new `dinghy` repo as submodule for the Makefile.
- `junk-annotate` now indicates by face attribute which packages are
  already installed.

### Changed

- The entire package was refactored to simplify the code.
- `junk--pack-p` was renamed to `junk--pack-package-p` to reflect the
  purpose of the function.

## [v0.1.0]

Initial version as an extraction of my config package.
