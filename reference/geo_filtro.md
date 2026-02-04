# Filtrar por departamentos y municipios

Función que filtra los datos de una enfermedad o evento por
departamentos y municipios.

## Usage

``` r
geo_filtro(data_event, dpto = NULL, mpio = NULL)
```

## Arguments

- data_event:

  Un \`data.frame\` con los datos de una enfermedad o evento.

- dpto:

  Un \`character\` (cadena de caracteres) o \`numeric\` (numérico) que
  contiene el nombre o código del departamento; valor por defecto
  \`NULL\`.

- mpio:

  Un \`character\` (cadena de caracteres) o \`numeric\` (numérico) que
  contiene el nombre o código del municipio; su valor por defecto es
  \`NULL\`.

## Value

Un \`data.frame\` con los datos filtrados con la enfermedad,
departamentos y municipios seleccionados.

## Details

Para ver la lista de valores aceptados para los parámetros \`dpto\` y
\`mpio\`, ejecuta la función \[\`import_geo_cods()\`\]. Se recomienda
consultar esta lista para evitar errores por nombres no válidos.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
geo_filtro(data_event = data_limpia, dpto = "ANTIOQUIA")
#> # A tibble: 12 × 73
#>    consecutive cod_eve fec_not    semana ano   cod_pre    cod_sub  edad uni_med
#>    <chr>       <chr>   <date>     <chr>  <chr> <chr>      <chr>   <dbl>   <dbl>
#>  1 7322720     210     2020-01-09 01     2020  0504509099 37          1       1
#>  2 7249231     210     2020-02-07 04     2020  1100123118 01         64       1
#>  3 7234649     210     2020-02-06 05     2020  0500102126 01         57       1
#>  4 7233727     210     2020-02-20 08     2020  0549004811 01         13       1
#>  5 7216310     210     2020-03-06 09     2020  0500102178 10         25       1
#>  6 7219710     210     2020-03-20 10     2020  0512909099 03         48       1
#>  7 7315084     210     2020-03-16 11     2020  1557200807 01          8       1
#>  8 7359390     210     2020-03-20 12     2020  0536009099 24         27       1
#>  9 7299329     210     2020-03-30 13     2020  0580904349 01         13       1
#> 10 7328153     210     2020-04-06 14     2020  0500102104 01         77       1
#> 11 7303305     210     2020-04-08 15     2020  0573606125 01         11       1
#> 12 7396875     210     2020-04-20 16     2020  0583702287 01          2       1
#> # ℹ 64 more variables: nacionalidad <chr>, nombre_nacionalidad <chr>,
#> #   sexo <chr>, cod_pais_o <chr>, cod_dpto_o <chr>, cod_mun_o <chr>,
#> #   area <chr>, ocupacion <chr>, tip_ss <chr>, cod_ase <chr>, per_etn <chr>,
#> #   gru_pob <chr>, nom_grupo <lgl>, estrato <chr>, gp_discapa <chr>,
#> #   gp_desplaz <chr>, gp_migrant <chr>, gp_carcela <chr>, gp_gestan <chr>,
#> #   sem_ges <lgl>, gp_indigen <chr>, gp_pobicfb <chr>, gp_mad_com <chr>,
#> #   gp_desmovi <chr>, gp_psiquia <chr>, gp_vic_vio <chr>, gp_otros <chr>, …
geo_filtro(data_event = data_limpia, dpto = "ANTIOQUIA", mpio = "MEDELLIN")
#> # A tibble: 4 × 73
#>   consecutive cod_eve fec_not    semana ano   cod_pre    cod_sub  edad uni_med
#>   <chr>       <chr>   <date>     <chr>  <chr> <chr>      <chr>   <dbl>   <dbl>
#> 1 7234649     210     2020-02-06 05     2020  0500102126 01         57       1
#> 2 7216310     210     2020-03-06 09     2020  0500102178 10         25       1
#> 3 7359390     210     2020-03-20 12     2020  0536009099 24         27       1
#> 4 7328153     210     2020-04-06 14     2020  0500102104 01         77       1
#> # ℹ 64 more variables: nacionalidad <chr>, nombre_nacionalidad <chr>,
#> #   sexo <chr>, cod_pais_o <chr>, cod_dpto_o <chr>, cod_mun_o <chr>,
#> #   area <chr>, ocupacion <chr>, tip_ss <chr>, cod_ase <chr>, per_etn <chr>,
#> #   gru_pob <chr>, nom_grupo <lgl>, estrato <chr>, gp_discapa <chr>,
#> #   gp_desplaz <chr>, gp_migrant <chr>, gp_carcela <chr>, gp_gestan <chr>,
#> #   sem_ges <lgl>, gp_indigen <chr>, gp_pobicfb <chr>, gp_mad_com <chr>,
#> #   gp_desmovi <chr>, gp_psiquia <chr>, gp_vic_vio <chr>, gp_otros <chr>, …
geo_filtro(data_event = data_limpia, dpto = "05")
#> # A tibble: 12 × 73
#>    consecutive cod_eve fec_not    semana ano   cod_pre    cod_sub  edad uni_med
#>    <chr>       <chr>   <date>     <chr>  <chr> <chr>      <chr>   <dbl>   <dbl>
#>  1 7322720     210     2020-01-09 01     2020  0504509099 37          1       1
#>  2 7249231     210     2020-02-07 04     2020  1100123118 01         64       1
#>  3 7234649     210     2020-02-06 05     2020  0500102126 01         57       1
#>  4 7233727     210     2020-02-20 08     2020  0549004811 01         13       1
#>  5 7216310     210     2020-03-06 09     2020  0500102178 10         25       1
#>  6 7219710     210     2020-03-20 10     2020  0512909099 03         48       1
#>  7 7315084     210     2020-03-16 11     2020  1557200807 01          8       1
#>  8 7359390     210     2020-03-20 12     2020  0536009099 24         27       1
#>  9 7299329     210     2020-03-30 13     2020  0580904349 01         13       1
#> 10 7328153     210     2020-04-06 14     2020  0500102104 01         77       1
#> 11 7303305     210     2020-04-08 15     2020  0573606125 01         11       1
#> 12 7396875     210     2020-04-20 16     2020  0583702287 01          2       1
#> # ℹ 64 more variables: nacionalidad <chr>, nombre_nacionalidad <chr>,
#> #   sexo <chr>, cod_pais_o <chr>, cod_dpto_o <chr>, cod_mun_o <chr>,
#> #   area <chr>, ocupacion <chr>, tip_ss <chr>, cod_ase <chr>, per_etn <chr>,
#> #   gru_pob <chr>, nom_grupo <lgl>, estrato <chr>, gp_discapa <chr>,
#> #   gp_desplaz <chr>, gp_migrant <chr>, gp_carcela <chr>, gp_gestan <chr>,
#> #   sem_ges <lgl>, gp_indigen <chr>, gp_pobicfb <chr>, gp_mad_com <chr>,
#> #   gp_desmovi <chr>, gp_psiquia <chr>, gp_vic_vio <chr>, gp_otros <chr>, …
geo_filtro(data_event = data_limpia, dpto = "05", mpio = "05001")
#> # A tibble: 4 × 73
#>   consecutive cod_eve fec_not    semana ano   cod_pre    cod_sub  edad uni_med
#>   <chr>       <chr>   <date>     <chr>  <chr> <chr>      <chr>   <dbl>   <dbl>
#> 1 7234649     210     2020-02-06 05     2020  0500102126 01         57       1
#> 2 7216310     210     2020-03-06 09     2020  0500102178 10         25       1
#> 3 7359390     210     2020-03-20 12     2020  0536009099 24         27       1
#> 4 7328153     210     2020-04-06 14     2020  0500102104 01         77       1
#> # ℹ 64 more variables: nacionalidad <chr>, nombre_nacionalidad <chr>,
#> #   sexo <chr>, cod_pais_o <chr>, cod_dpto_o <chr>, cod_mun_o <chr>,
#> #   area <chr>, ocupacion <chr>, tip_ss <chr>, cod_ase <chr>, per_etn <chr>,
#> #   gru_pob <chr>, nom_grupo <lgl>, estrato <chr>, gp_discapa <chr>,
#> #   gp_desplaz <chr>, gp_migrant <chr>, gp_carcela <chr>, gp_gestan <chr>,
#> #   sem_ges <lgl>, gp_indigen <chr>, gp_pobicfb <chr>, gp_mad_com <chr>,
#> #   gp_desmovi <chr>, gp_psiquia <chr>, gp_vic_vio <chr>, gp_otros <chr>, …
geo_filtro(data_event = data_limpia, dpto = 05, mpio = 05001)
#> # A tibble: 4 × 73
#>   consecutive cod_eve fec_not    semana ano   cod_pre    cod_sub  edad uni_med
#>   <chr>       <chr>   <date>     <chr>  <chr> <chr>      <chr>   <dbl>   <dbl>
#> 1 7234649     210     2020-02-06 05     2020  0500102126 01         57       1
#> 2 7216310     210     2020-03-06 09     2020  0500102178 10         25       1
#> 3 7359390     210     2020-03-20 12     2020  0536009099 24         27       1
#> 4 7328153     210     2020-04-06 14     2020  0500102104 01         77       1
#> # ℹ 64 more variables: nacionalidad <chr>, nombre_nacionalidad <chr>,
#> #   sexo <chr>, cod_pais_o <chr>, cod_dpto_o <chr>, cod_mun_o <chr>,
#> #   area <chr>, ocupacion <chr>, tip_ss <chr>, cod_ase <chr>, per_etn <chr>,
#> #   gru_pob <chr>, nom_grupo <lgl>, estrato <chr>, gp_discapa <chr>,
#> #   gp_desplaz <chr>, gp_migrant <chr>, gp_carcela <chr>, gp_gestan <chr>,
#> #   sem_ges <lgl>, gp_indigen <chr>, gp_pobicfb <chr>, gp_mad_com <chr>,
#> #   gp_desmovi <chr>, gp_psiquia <chr>, gp_vic_vio <chr>, gp_otros <chr>, …
geo_filtro(data_event = data_limpia, dpto = 05, mpio = 001)
#> # A tibble: 4 × 73
#>   consecutive cod_eve fec_not    semana ano   cod_pre    cod_sub  edad uni_med
#>   <chr>       <chr>   <date>     <chr>  <chr> <chr>      <chr>   <dbl>   <dbl>
#> 1 7234649     210     2020-02-06 05     2020  0500102126 01         57       1
#> 2 7216310     210     2020-03-06 09     2020  0500102178 10         25       1
#> 3 7359390     210     2020-03-20 12     2020  0536009099 24         27       1
#> 4 7328153     210     2020-04-06 14     2020  0500102104 01         77       1
#> # ℹ 64 more variables: nacionalidad <chr>, nombre_nacionalidad <chr>,
#> #   sexo <chr>, cod_pais_o <chr>, cod_dpto_o <chr>, cod_mun_o <chr>,
#> #   area <chr>, ocupacion <chr>, tip_ss <chr>, cod_ase <chr>, per_etn <chr>,
#> #   gru_pob <chr>, nom_grupo <lgl>, estrato <chr>, gp_discapa <chr>,
#> #   gp_desplaz <chr>, gp_migrant <chr>, gp_carcela <chr>, gp_gestan <chr>,
#> #   sem_ges <lgl>, gp_indigen <chr>, gp_pobicfb <chr>, gp_mad_com <chr>,
#> #   gp_desmovi <chr>, gp_psiquia <chr>, gp_vic_vio <chr>, gp_otros <chr>, …
```
