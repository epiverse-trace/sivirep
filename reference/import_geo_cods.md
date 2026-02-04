# Importar datos geográficos de Colombia

Función que importa los nombres y códigos de los departamentos y
municipios de Colombia a través de una URL.

Esta función requiere acceso a Internet.

## Usage

``` r
import_geo_cods(descargar = FALSE)
```

## Arguments

- descargar:

  Un \`logical\` (\`TRUE\` o \`FALSE\`) que indica si los datos deben
  descargarse desde la API de datos abiertos de Colombia. Su valor por
  defecto es \`FALSE\`, ya que sivirep ya incluye una copia local de
  dichos datos.

## Value

Un \`data.frame\` con los nombres y códigos de los departamentos y
municipios de Colombia.

## Examples

``` r
# \donttest{
import_geo_cods(descargar = FALSE)
#> # A tibble: 1,121 × 5
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
#> # ℹ 1,111 more rows
#> # ℹ 1 more variable: tipo_municipio_isla_area_no_municipalizada <chr>
# }
```
