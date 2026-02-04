# Importar la población a riesgo de un evento o enfermedad

Función que importa la población a riesgo de un evento o enfermedad para
un año específico.

Esta función requiere acceso a Internet.

## Usage

``` r
import_pob_riesgo(event, year, ruta_dir = NULL, cache = FALSE)
```

## Arguments

- event:

  Un \`character\` (cadena de caracteres) o un \`numeric\` (numérico)
  con el nombre o código de la enfermedad o evento.

- year:

  Un \`numeric\` (numérico) con el año deseado de la población a riesgo.

- ruta_dir:

  Un \`character\` (cadena de caracteres) que especifica la ruta del
  directorio donde se almacenarán la población a riesgo o las
  proyecciones poblacionales DANE. Su valor por defecto es \`NULL\`.

- cache:

  Un \`logical\` (\`TRUE\` o \`FALSE\`) que indica si la población a
  riesgo o las proyecciones poblacionales DANE descargadas deben ser
  almacenados en caché. Su valor por defecto es \`FALSE\`.

## Value

Un \`data.frame\` con la población a riesgo de un año específico.

## Examples

``` r
# \donttest{
import_pob_riesgo(event = "Dengue", year = 2020, ruta_dir = tempdir())
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
if (interactive()) {
  import_pob_riesgo(event = "Dengue", year = 2020, cache = TRUE)
  }
# }
```
