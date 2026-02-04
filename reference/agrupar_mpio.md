# Agrupar por municipios y casos

Función que agrupa los datos de una enfermedad o evento por código de
municipios y número de casos.

## Usage

``` r
agrupar_mpio(
  data_event,
  dpto = NULL,
  col_mpio = "cod_mun_o",
  porcentaje = FALSE
)
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento.

- dpto:

  Un \`character\` (cadena de caracteres) o \`numeric\` (numérico) que
  contiene el nombre del departamento; su valor por defecto es \`NULL\`.

- col_mpio:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene los códigos de los municipios en los datos de la
  enfermedad o evento; su valor por defecto es \`"cod_mun_o"\`.

- porcentaje:

  Un \`logical\` (TRUE o FALSE) que indica si se debe agregar una
  columna con el porcentaje de casos; su valor por defecto es \`FALSE\`.

## Value

Un \`data.frame\` con los datos de la enfermedad o evento agrupados por
códigos de municipios y número de casos.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
agrupar_mpio(
  data_event = data_limpia,
  dpto = "ANTIOQUIA",
  col_mpio = "cod_mun_o",
  porcentaje = FALSE
)
#> # A tibble: 9 × 8
#>   cod_dpto_o departamento_ocurrencia cod_mun_o municipio_ocurrencia cod_eve
#>   <chr>      <chr>                   <chr>     <chr>                <chr>  
#> 1 05         ANTIOQUIA               05001     MEDELLIN             210    
#> 2 05         ANTIOQUIA               05045     APARTADO             210    
#> 3 05         ANTIOQUIA               05129     CALDAS               210    
#> 4 05         ANTIOQUIA               05490     NECOCLI              210    
#> 5 05         ANTIOQUIA               05591     PUERTO TRIUNFO       210    
#> 6 05         ANTIOQUIA               05659     SAN JUAN DE URABA    210    
#> 7 05         ANTIOQUIA               05736     SEGOVIA              210    
#> 8 05         ANTIOQUIA               05809     TITIRIBI             210    
#> 9 05         ANTIOQUIA               05837     TURBO                210    
#> # ℹ 3 more variables: nombre_evento <chr>, ano <chr>, casos <int>
agrupar_mpio(
  data_event = data_limpia,
  dpto = "05",
  col_mpio = "cod_mun_o",
  porcentaje = FALSE
)
#> # A tibble: 9 × 8
#>   cod_dpto_o departamento_ocurrencia cod_mun_o municipio_ocurrencia cod_eve
#>   <chr>      <chr>                   <chr>     <chr>                <chr>  
#> 1 05         ANTIOQUIA               05001     MEDELLIN             210    
#> 2 05         ANTIOQUIA               05045     APARTADO             210    
#> 3 05         ANTIOQUIA               05129     CALDAS               210    
#> 4 05         ANTIOQUIA               05490     NECOCLI              210    
#> 5 05         ANTIOQUIA               05591     PUERTO TRIUNFO       210    
#> 6 05         ANTIOQUIA               05659     SAN JUAN DE URABA    210    
#> 7 05         ANTIOQUIA               05736     SEGOVIA              210    
#> 8 05         ANTIOQUIA               05809     TITIRIBI             210    
#> 9 05         ANTIOQUIA               05837     TURBO                210    
#> # ℹ 3 more variables: nombre_evento <chr>, ano <chr>, casos <int>
agrupar_mpio(
  data_event = data_limpia,
  dpto = 05,
  col_mpio = "cod_mun_o",
  porcentaje = TRUE
)
#> # A tibble: 9 × 9
#>   cod_dpto_o departamento_ocurrencia cod_mun_o municipio_ocurrencia cod_eve
#>   <chr>      <chr>                   <chr>     <chr>                <chr>  
#> 1 05         ANTIOQUIA               05001     MEDELLIN             210    
#> 2 05         ANTIOQUIA               05045     APARTADO             210    
#> 3 05         ANTIOQUIA               05129     CALDAS               210    
#> 4 05         ANTIOQUIA               05490     NECOCLI              210    
#> 5 05         ANTIOQUIA               05591     PUERTO TRIUNFO       210    
#> 6 05         ANTIOQUIA               05659     SAN JUAN DE URABA    210    
#> 7 05         ANTIOQUIA               05736     SEGOVIA              210    
#> 8 05         ANTIOQUIA               05809     TITIRIBI             210    
#> 9 05         ANTIOQUIA               05837     TURBO                210    
#> # ℹ 4 more variables: nombre_evento <chr>, ano <chr>, casos <int>,
#> #   porcentaje <dbl>
```
