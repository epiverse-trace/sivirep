# sivirep 0.0.1

This is the initial version of the package. It contains four modules which are as follows:

-   `import_data`: contains the functions and logic to import data from SIVIGILA
-   `checking_data`: groups the functions that allow generating distributions of cases
-   `cleaning_data`: contains the set of functions that enable data cleaning
-   `plotting_data`: contains the set of functions that allow generating distribution plots

# sivirep 0.0.9

This version includes a complete refactoring of the package, an R Markdown template called `Reporte Basico` to facilitate report generation for users, bug fixes, and various improvements identified from user testing conducted with the Secretariat of Health of Bogotá, health professionals and students from the Technological University of Chocó, and expert reviews carried out by the Colombia's National Institute of Health.

The modules represent the data flow that the user must follow to generate the automated report, and the functions represent the key variables to be considered for generating a descriptive analysis that facilitates decision-making and the obtaining of key indicators defined in the protocols of events or diseases by the Colombia's National Institute of Health.

# sivirep 1.0.0

This major release introduces a variety of significant new features and addresses bugs identified in previous versions of the package. The updates are based on usability testing and the expert review process conducted with Colombia's National Institute of Health.

## New features

### Incidence:

Set of functions to import, group, and plot the incidence calculations of a specific disease or event for Colombia, departments, or municipalities

-   `import_pob_incidencia()`
-   `import_pob_proyecciones()`
-   `import_pob_riesgo()`
-   `calcular_incidencia()`
-   `calcular_incidencia_geo()`
-   `calcular_incidencia_sex()`
-   `plot_tabla_incidencia_geo()`
-   `plot_tabla_incidencia_sex()`

### Ethnicity

Functions to generated the cases distribution, and plot of the ethnicity groups related to a specific disease or event

-   `agrupar_per_etn()`
-   `plot_per_etn()`

### Intial classification of cases

Functions to group, and plot the initial classification of cases

-   `agrupar_tipo_caso()`
-   `plot_tipo_caso()`
-   `plot_tipo_caso_years()`

### Years (restrospective)

Functions to generate the distribution of cases, and plot across multiple years

-   `agrupar_years()`
-   `plot_years()`

### Events

Functions to generate the distribution of cases by event type

-   `agrupar_events()`
-   `plot_tabla_tipos_event()`

### Geographic Area

Functions to generate and plot the distribution of cases by geographic area, including top departments or municipalities.

-   `agrupar_area_geo()`
-   `agrupar_top_area_geo()`
-   `plot_area_geo`
-   `plot_top_area_geo`

## Report template:

The report template `Reporte Evento` now includes five new sections called:
-   `Distribución de casos` 
-   `Distribución de casos por clasificación`
-   `Distribución por área geográfica`
-   `Distribución por pertenencia étnica` 
-   `Incidencia`

# sivirep 1.0.1

This patch release fixes the following items::

-   `es_ES` was replaced with `es-ES` in the `DESCRIPTION` file.
  
-   Updated function examples that require data import and 
    use the `cache` parameter to prevent an increase in the package's size 
    on CRAN.
    
-   Rename the report template `Reporte Basico` to `Reporte Evento`

