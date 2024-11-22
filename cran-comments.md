## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.

* sivirep currently has its documentation, function names, and website in 
  Spanish, as user testing in various regions of Colombia, such as the Chocó 
  region, revealed that having these components in English posed a 
  significant barrier. Many users in these areas are not proficient in English, 
  which increases learning curves and limits the adoption of the tool. This 
  package is expected to be used by Health Departments and Professionals 
  throughout the country to facilitate the generation and access to 
  epidemiological reports from SIVIGILA (National Public Health Surveillance 
  System).

## Resubmission

This is a resubmission. In this version:

* Updated `DESCRIPTION` file to use language tags compliant with RFC 5646. 
  Specifically, `es_ES` was replaced with `es-ES` as recommended.
  
* Implemented `tempdir()` for function examples that require data import and 
  use the `cache` parameter. This change resolves additional `donttest` issues 
  related to package's size.
