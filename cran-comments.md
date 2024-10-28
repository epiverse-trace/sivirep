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

* The links in the README file to the pages of Colombia's National Institute of
  Health have been modified to avoid DNS or regional access issues.

* The examples for the functions `import_data_event`, `list_events` and
  `plot_year` are wrapped in a `dontrun` tag because they connect to the
  SIVIGILA API. We have observed that, in some regions, it is not possible to
  establish a connection with the API for security reasons. These functions
  cannot be wrapped in a `try-catch` block, as this may lead confusion in our
  users.
