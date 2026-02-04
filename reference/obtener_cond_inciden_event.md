# Obtener las condiciones para calcular la incidencia de una enfermedad o evento

Función que obtiene las condiciones del numerador, denominador y
coeficiente de múltiplicación para calcular la incidencia de un evento.

## Usage

``` r
obtener_cond_inciden_event(cod_eve)
```

## Arguments

- cod_eve:

  Un \`numeric\` (numérico) o \`character\` (cadena de caracteres) que
  contiene el código de una enfermedad o evento.

## Value

Un \`data.frame\` con las condiciones para calcular la incidencia de una
enfermedad o evento.

## Examples

``` r
obtener_cond_inciden_event(cod_eve = 210)
#> # A tibble: 1 × 6
#>   cod_eve numerador condiciones_numerador denominador condiciones_denominador
#>     <dbl> <chr>     <chr>                 <chr>       <chr>                  
#> 1     210 casos     NA                    riesgo      NA                     
#> # ℹ 1 more variable: coeficiente <int>
```
