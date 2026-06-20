## Test environments

* local Ubuntu 24.04.3 LTS, R 4.3.3

## R CMD check results

0 errors | 0 warnings | 4 notes

## Notes

* New submission.
* This resubmission removes the obsolete C++11 standard request and updates
  DESCRIPTION wording flagged by the previous incoming checks.
* Installed size is 19.7 MB. The largest component is the locally compiled
  shared library built from the package C++ sources (`libs`, 15.0 MB), followed
  by bundled data (`data`, 2.0 MB). The source package does not ship compiled
  objects; the local shared object includes debug symbols.
* CRAN ships the short/core vignettes. Longer workflow articles are excluded
  from the CRAN build and rendered on the package site to keep CRAN build time
  and installed documentation size controlled.
* Unable to verify current time. This appears to be a local check-environment
  issue.
* Non-portable compiler flag `-mno-omit-leaf-frame-pointer`. The package does
  not set this flag; it comes from the local compiler/R configuration.
* HTML validation was run locally with `tidy`; no tidy errors were found. Tidy
  reported warnings from generated pkgdown/Pandoc HTML5 markup (for example
  table alignment attributes and empty spans).
