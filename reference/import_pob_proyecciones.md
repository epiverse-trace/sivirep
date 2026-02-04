# Importar las proyecciones DANE del año 2005 hasta el 2035

Función que importa las proyecciones poblacionales DANE desde el año
2005 hasta el 2035.

Esta función requiere acceso a Internet.

## Usage

``` r
import_pob_proyecciones(year, ruta_dir = NULL, cache = FALSE)
```

## Arguments

- year:

  Un \`numeric\` (numérico) con el año de las proyecciones poblacionales
  DANE que desea importar.

- ruta_dir:

  Un \`character\` (cadena de caracteres) que especifica la ruta del
  directorio donde se almacenarán la población a riesgo o las
  proyecciones poblacionales DANE. Su valor por defecto es \`NULL\`.

- cache:

  Un \`logical\` (\`TRUE\` o \`FALSE\`) que indica si la población a
  riesgo o las proyecciones poblacionales DANE descargadas deben ser
  almacenados en caché. Su valor por defecto es \`FALSE\`.

## Value

Un \`data.frame\` con las proyecciones poblacionales DANE.

## Examples

``` r
# \donttest{
import_pob_proyecciones(year = 2020, ruta_dir = tempdir())
#> # A tibble: 289,476 × 10
#>    dp    dpnom     dpmp   mpio    ano area_geografica grupo_edad hombres mujeres
#>    <chr> <chr>     <chr>  <chr> <dbl> <chr>           <chr>        <dbl>   <dbl>
#>  1 05    Antioquia Medel… 05001  2020 Cabecera Munic… 0            14037   13366
#>  2 05    Antioquia Medel… 05001  2020 Cabecera Munic… 1            14299   13705
#>  3 05    Antioquia Medel… 05001  2020 Cabecera Munic… 2            14543   13995
#>  4 05    Antioquia Medel… 05001  2020 Cabecera Munic… 3            14689   14120
#>  5 05    Antioquia Medel… 05001  2020 Cabecera Munic… 4            14796   14218
#>  6 05    Antioquia Medel… 05001  2020 Cabecera Munic… 5            14890   14318
#>  7 05    Antioquia Medel… 05001  2020 Cabecera Munic… 6            14962   14421
#>  8 05    Antioquia Medel… 05001  2020 Cabecera Munic… 7            15036   14529
#>  9 05    Antioquia Medel… 05001  2020 Cabecera Munic… 8            15135   14664
#> 10 05    Antioquia Medel… 05001  2020 Cabecera Munic… 9            15263   14811
#> # ℹ 289,466 more rows
#> # ℹ 1 more variable: total <dbl>
if (interactive()) {
  import_pob_proyecciones(year = 2020, cache = TRUE)
  }
# }
```
