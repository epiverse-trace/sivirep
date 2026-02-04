# Generar tabla con la incidencia

Función que genera la tabla con la incidencia según la distribución
geográfica.

## Usage

``` r
plot_tabla_incidencia_geo(data_agrupada, col_geo = NULL)
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento
  agrupados por departamento o municipio.

- col_geo:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene los nombres de los departamentos o municipios en los
  datos agrupados de la enfermedad o evento; su valor por defecto es
  \`NULL\`.

## Value

Una \`kable\` (tabla gráfica) con la incidencia según distribución
geográfica.

## Examples

``` r
# \donttest{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
data_agrupada <- agrupar_mpio(data_limpia, dpto = "Antioquia")
incidencia_mpios <- calcular_incidencia_geo(
  data_agrupada =
    data_agrupada,
  ruta_dir = tempdir()
)
#> Las incidencias se calcularon con la poblacion a riesgo definida por el Ministerio de Salud para el 2020
plot_tabla_incidencia_geo(
  data_agrupada = incidencia_mpios$data_incidencia,
  col_geo = "municipio_ocurrencia"
)
#> <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
#> <caption>Incidencia por geografía de ocurrencia por 100000 habitantes</caption>
#>  <thead>
#>   <tr>
#>    <th style="text-align:center;color: white !important;background-color: rgba(34, 116, 187, 255) !important;"> Código </th>
#>    <th style="text-align:center;color: white !important;background-color: rgba(34, 116, 187, 255) !important;"> Municipio </th>
#>    <th style="text-align:center;color: white !important;background-color: rgba(34, 116, 187, 255) !important;"> Incidencia </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:center;"> 05809 </td>
#>    <td style="text-align:center;"> Titiribi </td>
#>    <td style="text-align:center;"> 9.33 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:center;"> 05591 </td>
#>    <td style="text-align:center;"> Puerto Triunfo </td>
#>    <td style="text-align:center;"> 5.27 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:center;"> 05659 </td>
#>    <td style="text-align:center;"> San Juan De Uraba </td>
#>    <td style="text-align:center;"> 4.77 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:center;"> 05736 </td>
#>    <td style="text-align:center;"> Segovia </td>
#>    <td style="text-align:center;"> 2.54 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:center;"> 05490 </td>
#>    <td style="text-align:center;"> Necocli </td>
#>    <td style="text-align:center;"> 2.27 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:center;"> 05129 </td>
#>    <td style="text-align:center;"> Caldas </td>
#>    <td style="text-align:center;"> 1.48 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:center;"> 05045 </td>
#>    <td style="text-align:center;"> Apartado </td>
#>    <td style="text-align:center;"> 0.78 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:center;"> 05837 </td>
#>    <td style="text-align:center;"> Turbo </td>
#>    <td style="text-align:center;"> 0.77 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:center;"> 05001 </td>
#>    <td style="text-align:center;"> Medellin </td>
#>    <td style="text-align:center;"> 0.16 </td>
#>   </tr>
#> </tbody>
#> </table>
# }
```
