## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.

* Updated `DESCRIPTION` file to use language tags compliant with RFC 5646. 
  Specifically, `es_ES` was replaced with `es-ES` as recommended.
  
* Implemented `tempdir()` for function examples that require data import and 
  improved the use of `cache` parameter. These changes resolves additional 
  `donttest` issues related to package's size on CRAN.
