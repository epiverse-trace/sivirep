# Generar tabla con la incidencia por sexo

Función que genera la tabla con la incidencia por sexo.

## Usage

``` r
plot_tabla_incidencia_sex(data_agrupada, col_sex = "sexo")
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento
  agrupados por departamento o municipio.

- col_sex:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene el sexo en los datos agrupados de la enfermedad o evento;
  su valor por defecto es \`"sexo"\`.

## Value

Una \`kable\` (tabla gráfica) con la incidencia por sexo.

## Examples

``` r
# \donttest{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
data_agrupada_sex <- agrupar_sex(data_limpia)
incidencia_mpios <-
  calcular_incidencia_sex(
    data_agrupada = data_agrupada_sex,
    dpto = "Antioquia",
    ruta_dir = tempdir()
  )
#> Las incidencias se calcularon con las proyecciones poblacionales DANE. Si usted cuenta con la poblacion a riesgo definida por el Ministerio de Salud para el 2020 puede hacer uso de ella, asignandola en el argumento data_incidencia de la funcion
plot_tabla_incidencia_sex(
  data_agrupada = incidencia_mpios$data_incidencia,
  col_sex = "sexo"
)
#> <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
#> <caption>Incidencia por sexo por 100000 habitantes</caption>
#>  <thead>
#>   <tr>
#>    <th style="text-align:center;color: white !important;background-color: rgba(34, 116, 187, 255) !important;"> Código </th>
#>    <th style="text-align:center;color: white !important;background-color: rgba(34, 116, 187, 255) !important;"> Evento </th>
#>    <th style="text-align:center;color: white !important;background-color: rgba(34, 116, 187, 255) !important;"> Sexo </th>
#>    <th style="text-align:center;color: white !important;background-color: rgba(34, 116, 187, 255) !important;"> Incidencia </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:center;"> 210 </td>
#>    <td style="text-align:center;"> Dengue </td>
#>    <td style="text-align:center;"> M </td>
#>    <td style="text-align:center;"> 0.78 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:center;"> 210 </td>
#>    <td style="text-align:center;"> Dengue </td>
#>    <td style="text-align:center;"> F </td>
#>    <td style="text-align:center;"> 0.64 </td>
#>   </tr>
#> </tbody>
#> </table>
# }
```
