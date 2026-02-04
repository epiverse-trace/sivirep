# Obtener el nombre de un departamento de Colombia

Función que obtiene el nombre de un departamento de Colombia a partir de
su código geográfico.

## Usage

``` r
obtener_nombre_dpto(data_geo, cod_dpto)
```

## Arguments

- data_geo:

  Un \`data.frame\` que contiene los códigos geográficos (departamentos
  y municipios de Colombia).

- cod_dpto:

  Un \`numeric\` (numérico) o \`character\` (cadena de caracteres) que
  contiene el código del departamento.

## Value

Un \`character\` (cadena de caracteres) con el nombre del departamento.

## Examples

``` r
data_geo <- import_geo_cods()
obtener_nombre_dpto(data_geo,
  cod_dpto = "05"
)
#> [1] "antioquia"
obtener_nombre_dpto(data_geo,
  cod_dpto = 05
)
#> [1] "antioquia"
obtener_nombre_dpto(data_geo,
  cod_dpto = 5
)
#> [1] "antioquia"
```
