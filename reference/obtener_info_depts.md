# Obtener información geográfica de los datos de la enfermedad o evento

Función que obtiene la información geográfica de los datos de la
enfermedad o evento.

## Usage

``` r
obtener_info_depts(dpto = NULL, mpio = NULL)
```

## Arguments

- dpto:

  Un \`character\` (cadena de caracteres) o \`numeric\` (numérico) que
  contiene el nombre o código del departamento; su valor por defecto es
  \`NULL\`.

- mpio:

  Un \`character\` (cadena de caracteres) o \`numeric\` (numérico) que
  contiene el nombre o código del municipio; su valor por defecto es
  \`NULL\`.

## Value

Un \`data.frame\` con la información geográfica de los datos de la
enfermedad o evento.

## Examples

``` r
obtener_info_depts(dpto = "ANTIOQUIA")
#> # A tibble: 125 × 5
#>    codigo_departamento codigo_municipio nombre_departamento nombre_municipio
#>    <chr>               <chr>            <chr>               <chr>           
#>  1 05                  05001            antioquia           medellin        
#>  2 05                  05002            antioquia           abejorral       
#>  3 05                  05004            antioquia           abriaqui        
#>  4 05                  05021            antioquia           alejandria      
#>  5 05                  05030            antioquia           amaga           
#>  6 05                  05031            antioquia           amalfi          
#>  7 05                  05034            antioquia           andes           
#>  8 05                  05036            antioquia           angelopolis     
#>  9 05                  05038            antioquia           angostura       
#> 10 05                  05040            antioquia           anori           
#> # ℹ 115 more rows
#> # ℹ 1 more variable: tipo_municipio_isla_area_no_municipalizada <chr>
obtener_info_depts(dpto = "ANTIOQUIA", mpio = "MEDELLIN")
#> # A tibble: 1 × 5
#>   codigo_departamento codigo_municipio nombre_departamento nombre_municipio
#>   <chr>               <chr>            <chr>               <chr>           
#> 1 05                  05001            antioquia           medellin        
#> # ℹ 1 more variable: tipo_municipio_isla_area_no_municipalizada <chr>
obtener_info_depts(dpto = "05")
#> # A tibble: 125 × 5
#>    codigo_departamento codigo_municipio nombre_departamento nombre_municipio
#>    <chr>               <chr>            <chr>               <chr>           
#>  1 05                  05001            antioquia           medellin        
#>  2 05                  05002            antioquia           abejorral       
#>  3 05                  05004            antioquia           abriaqui        
#>  4 05                  05021            antioquia           alejandria      
#>  5 05                  05030            antioquia           amaga           
#>  6 05                  05031            antioquia           amalfi          
#>  7 05                  05034            antioquia           andes           
#>  8 05                  05036            antioquia           angelopolis     
#>  9 05                  05038            antioquia           angostura       
#> 10 05                  05040            antioquia           anori           
#> # ℹ 115 more rows
#> # ℹ 1 more variable: tipo_municipio_isla_area_no_municipalizada <chr>
obtener_info_depts(dpto = "05", mpio = "05001")
#> # A tibble: 1 × 5
#>   codigo_departamento codigo_municipio nombre_departamento nombre_municipio
#>   <chr>               <chr>            <chr>               <chr>           
#> 1 05                  05001            antioquia           medellin        
#> # ℹ 1 more variable: tipo_municipio_isla_area_no_municipalizada <chr>
obtener_info_depts(dpto = 05, mpio = 05001)
#> # A tibble: 1 × 5
#>   codigo_departamento codigo_municipio nombre_departamento nombre_municipio
#>   <chr>               <chr>            <chr>               <chr>           
#> 1 05                  05001            antioquia           medellin        
#> # ℹ 1 more variable: tipo_municipio_isla_area_no_municipalizada <chr>
obtener_info_depts(dpto = 05, mpio = 001)
#> # A tibble: 1 × 5
#>   codigo_departamento codigo_municipio nombre_departamento nombre_municipio
#>   <chr>               <chr>            <chr>               <chr>           
#> 1 05                  05001            antioquia           medellin        
#> # ℹ 1 more variable: tipo_municipio_isla_area_no_municipalizada <chr>
obtener_info_depts(dpto = "bogota dc", mpio = "bogota dc")
#> # A tibble: 1 × 5
#>   codigo_departamento codigo_municipio nombre_departamento nombre_municipio
#>   <chr>               <chr>            <chr>               <chr>           
#> 1 11                  11001            bogota_dc           bogota_dc       
#> # ℹ 1 more variable: tipo_municipio_isla_area_no_municipalizada <chr>
```
