# Obtener columnas de ocurrencia geográfica de los datos de la enfermedad o evento

Función que obtiene las columnas de ocurrencia geográfica de los datos
de la enfermedad o evento.

## Usage

``` r
obtener_tip_ocurren_geo(cod_event = NULL, nombre_event = NULL)
```

## Arguments

- cod_event:

  Un \`numeric\` (numérico) o \`character\` (cadena de caracteres) que
  contiene el código de la enfermedad o evento.

- nombre_event:

  Un \`character\` (cadena de caracteres) con el nombre de la enfermedad
  o evento.

## Value

Un \`data.frame\` con las columnas de ocurrencia geográfica de los datos
de la enfermedad o evento.

## Examples

``` r
obtener_tip_ocurren_geo(cod_event = 210)
#> [1] "cod_dpto_o"              "departamento_ocurrencia"
#> [3] "cod_mun_o"               "municipio_ocurrencia"   
#> [5] "ocurrencia"             
```
