# Changelog

## [0.2.2](https://github.com/Walheimat/junk/compare/v0.2.1...v0.2.2) (2024-01-28)


### Bug Fixes

* **annotation:** don't break installing extras ([6bf633f](https://github.com/Walheimat/junk/commit/6bf633f70f980ee0ba72f3e7e2cc3f2014384553))


### Features

* **ci:** add semantic-release ([11df9e7](https://github.com/Walheimat/junk/commit/11df9e7f3f4b054d97c140e95111cce1b0d01ec8))
* **ci:** use 29.1 to build ([ad8a653](https://github.com/Walheimat/junk/commit/ad8a653e152b2001cab89275b383ca967aac2ce8))

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
