# Generar tabla con la distribución de casos por tipo de enfermedad o evento

Función que genera la tabla con la distribución de casos por tipo de
enfermedad o evento.

## Usage

``` r
plot_tabla_tipos_event(data_agrupada, col_event = "nombre_evento")
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento
  agrupados por tipo.

- col_event:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene el tipo de evento en los datos agrupados de la enfermedad
  o evento; su valor por defecto es \`"nombre_evento"\`.

## Value

Una \`kable\` (tabla gráfica) con la distribución de casos por tipo de
enfermedad o evento.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_agrupada <- agrupar_eventos(
  data_event = data_limpia,
  col_event = "cod_eve"
)
plot_tabla_tipos_event(data_agrupada,
  col_event = "nombre_evento"
)
#> <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
#> <caption>Distribución de casos por evento</caption>
#>  <thead>
#>   <tr>
#>    <th style="text-align:center;color: white !important;background-color: rgba(34, 116, 187, 255) !important;"> Código </th>
#>    <th style="text-align:center;color: white !important;background-color: rgba(34, 116, 187, 255) !important;"> Evento </th>
#>    <th style="text-align:center;color: white !important;background-color: rgba(34, 116, 187, 255) !important;"> Casos </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:center;"> 210 </td>
#>    <td style="text-align:center;"> Dengue </td>
#>    <td style="text-align:center;"> 47 </td>
#>   </tr>
#> </tbody>
#> </table>
```
