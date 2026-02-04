# Agrupar por la clasificación inicial del caso

Función que agrupa los casos por la clasificación inicial del caso.

## Usage

``` r
agrupar_tipo_caso(data_event, cols_tipo = "tip_cas")
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento.

- cols_tipo:

  Un \`character\` (cadena de caracteres) o \`array\` (arreglo) de
  \`character\` con el nombre de las columna(s) que contiene la
  clasificación inicial del caso en los datos de la enfermedad o evento;
  su valor por defecto es \`"tip_cas"\`.

## Value

Un \`data.frame\` con los datos de la enfermedad o evento agrupados por
la clasificación inicial del caso y/u otras variables como los años.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
agrupar_tipo_caso(
  data_event = data_limpia,
  cols_tipo = "tip_cas"
)
#> # A tibble: 3 × 6
#>   tip_cas cod_eve nombre_evento ano   casos nombre_tip_cas                    
#>   <chr>   <chr>   <chr>         <chr> <int> <chr>                             
#> 1 2       210     DENGUE        2020     37 Probable                          
#> 2 3       210     DENGUE        2020      9 Confirmado por laboratorio        
#> 3 5       210     DENGUE        2020      1 Confirmado por nexo epidemiologico
```
