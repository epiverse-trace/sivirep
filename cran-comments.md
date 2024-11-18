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

* Expanded the Description field to provide a more detailed overview of the 
  package's functionality, benefits and the methods it implements, as requested.
  
* Added a direct link to the web services used in the package, formatted with 
  angle brackets for auto-linking, following the recommended format.

* Replaced `\dontrun{}` with `\donttest{}` in the examples of the following 
  functions: `import_data_event()`, `list_events()` and `plot_year()`.

* Added `\donttest{}` in examples of `import_geo_codes` function, since it was
  involve data downloading and was the only function that did not have 
  this tag.

* Used `tempdir()` in examples and tests of functions that require data import
  for execution.

* In the internal function `obtener_ruta_dir()` of `R\utils.R`, the `cache` 
  parameter was added, and the functions that use it were modified to always 
  pass it. This parameter requests explicit confirmation from the user if they 
  want the data to be stored in the respective user directory, which is 
  obtained from `tools::R_user_dir()`. These files are cleaned up and actively 
  managed in accordance with CRAN policy.
