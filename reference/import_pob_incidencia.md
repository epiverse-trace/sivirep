# Importar la población para efectuar el cálculo de la incidencia

Función que importa la población a riesgo de un evento o enfermedad o
las proyecciones poblacionales DANE desde el año 2005 hasta el 2035.

Esta función requiere acceso a Internet.

## Usage

``` r
import_pob_incidencia(
  poblacion = c("riesgo", "proyecciones"),
  event,
  year,
  ruta_dir = NULL,
  cache = FALSE
)
```

## Arguments

- poblacion:

  Un \`character\` (cadena de caracteres) con el tipo de población que
  se desea importar. Puede ser \`"riesgo"\` para la población a riesgo
  del evento o \`"proyecciones"\` para las proyecciones poblacionales
  DANE; su valor por defecto es \`"riesgo"\`.

- event:

  Un \`character\` (cadena de caracteres) o un \`numeric\` (numérico)
  con el nombre o código de la enfermedad o evento. Es obligatorio para
  importar la población a riesgo.

- year:

  Un \`numeric\` (numérico) con el año deseado de la población a riesgo.
  Es obligatorio para importar la población a riesgo.

- ruta_dir:

  Un \`character\` (cadena de caracteres) que especifica la ruta del
  directorio donde se almacenarán la población a riesgo o las
  proyecciones poblacionales DANE. Su valor por defecto es \`NULL\`.

- cache:

  Un \`logical\` (\`TRUE\` o \`FALSE\`) que indica si la población a
  riesgo o las proyecciones poblacionales DANE descargadas deben ser
  almacenados en caché. Su valor por defecto es \`FALSE\`.

## Value

Un \`data.frame\` con la población a riesgo o las proyecciones
poblacionales DANE.

## Examples

``` r
 # \donttest{
# Importación proyecciones poblaciones DANE
if (interactive()) {
  import_pob_incidencia(poblacion = "proyecciones", year = 2020,
                        cache = TRUE)
}
# Importación población a riesgo de Dengue del año 2020
import_pob_incidencia(poblacion = "riesgo", event = "dengue", year = 2020,
                      ruta_dir = tempdir())
#> # A tibble: 1,122 × 5
#>    cod_dpto nombre_departamento cod_mpio nombre_municipio poblacion_riesgo_2020
#>    <chr>    <chr>               <chr>    <chr>                            <dbl>
#>  1 05       Antioquia           05001    Medellín                       2533424
#>  2 05       Antioquia           05002    Abejorral                            0
#>  3 05       Antioquia           05004    Abriaquí                             0
#>  4 05       Antioquia           05021    Alejandría                           0
#>  5 05       Antioquia           05030    Amagá                            31283
#>  6 05       Antioquia           05031    Amalfi                           15599
#>  7 05       Antioquia           05034    Andes                            44199
#>  8 05       Antioquia           05036    Angelópolis                       2331
#>  9 05       Antioquia           05038    Angostura                            0
#> 10 05       Antioquia           05040    Anorí                             8335
#> # ℹ 1,112 more rows
# }
```
