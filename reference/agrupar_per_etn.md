# Agrupar por la pertenencia étnica

Función que agrupa los casos por la pertenencia étnica.

## Usage

``` r
agrupar_per_etn(data_event, cols_etn = "per_etn", porcentaje = TRUE)
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento.

- cols_etn:

  Un \`character\` (cadena de caracteres) o un \`array\` de
  \`character\` con el nombre de la(s) columna(s) que contiene(n) la
  pertenencia étnica en los datos de la enfermedad o evento; su valor
  por defecto es \`"per_etn"\`

- porcentaje:

  Un \`logical\` (TRUE o FALSE) que indica si se debe agregar una
  columna con el porcentaje de casos; su valor por defecto es \`TRUE\`.

## Value

Un \`data.frame\` con los datos de la enfermedad o evento agrupados por
la pertenencia étnica.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
agrupar_per_etn(
  data_event = data_limpia,
  cols_etn = "per_etn"
)
#> # A tibble: 3 × 7
#>   per_etn cod_eve nombre_evento ano   casos porcentaje nombre_per_etn           
#>   <chr>   <chr>   <chr>         <chr> <int>      <dbl> <chr>                    
#> 1 1       210     DENGUE        2020      1       2.13 Indigena                 
#> 2 5       210     DENGUE        2020      1       2.13 Negro/ Mulato/ Afrocolom…
#> 3 6       210     DENGUE        2020     45      95.7  Otro                     
```
