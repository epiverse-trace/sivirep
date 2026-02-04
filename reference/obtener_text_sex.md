# Obtener el párrafo de la distribución de casos por sexo

Función que obtiene el párrafo descriptivo de la sección de distribución
de casos por sexo de la plantilla del reporte.

## Usage

``` r
obtener_text_sex(data_agrupada, year, figura)
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento
  agrupados por sexo.

- year:

  Un \`numeric\` (numérico) con el año de los datos agrupados por sexo.

- figura:

  Un \`numeric\` (numérico) con el número de la figura de la
  distribución de casos por sexo.

## Value

Un \`character\` (cadena de caracteres) con el párrafo descriptivo de la
distribución de casos por sexo.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_agrupada <- agrupar_sex(
  data_event = data_limpia,
  porcentaje = TRUE
)
obtener_text_sex(data_agrupada, year = 2020, figura = 3)
#> $text
#> [1] "En el total de casos para 2020 se observa una predominancia del sexo masculino con 53.19% respecto al sexo femenino con 46.81% (Ver Figura 3)."
#> 
#> $mayor
#> [1] "masculino" "53.19"    
#> 
#> $menor
#> [1] "femenino" "46.81"   
#> 
```
