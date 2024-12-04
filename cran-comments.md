## R CMD check results

0 errors | 0 warnings | 0 note

* Updated `DESCRIPTION` file to use language tags compliant with RFC 5646 and 
  CRAN policies. Specifically, `es_ES` was replaced with `es-ES` as recommended.
  
* Added `if(interactive())` to function examples that use `cache` parameter to 
  prevent an increase in the package's size on CRAN.
