# Calcular incidencia por sexo

Función que calcula la incidencia de una enfermedad o evento para todos
los departamentos de Colombia o los municipios de un departamento por
cada sexo.

## Usage

``` r
calcular_incidencia_sex(
  data_incidencia = NULL,
  ruta_dir = NULL,
  cache = FALSE,
  data_agrupada,
  year = NULL,
  dpto = NULL,
  mpio = NULL
)
```

## Arguments

- data_incidencia:

  Un \`data.frame\` que contiene las proyecciones poblacionales del
  DANE; su valor por defecto es \`NULL\`.

- ruta_dir:

  Un \`character\` (cadena de caracteres) que especifica la ruta del
  directorio donde se almacenarán la población a riesgo o las
  proyecciones poblacionales DANE. Su valor por defecto es \`NULL\`.

- cache:

  Un \`logical\` (\`TRUE\` o \`FALSE\`) que indica si la población a
  riesgo o las proyecciones poblacionales DANE descargadas deben ser
  almacenados en caché. Su valor por defecto es \`FALSE\`.

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad agrupados
  por departamento o municipio y número de casos.

- year:

  Un \`numeric\` (numérico) con el año que se debe tomar en la población
  a riesgo o en las proyecciones poblacionales DANE; su valor por
  defecto es \`NULL\`.

- dpto:

  Un \`character\` (cadena de caracteres) o \`numeric\` (numérico) que
  contiene el código o nombre del departamento; su valor por defecto es
  \`NULL\`.

- mpio:

  Un \`character\` (cadena de caracteres) o \`numeric\` (numérico) que
  contiene el código o nombre del municipio; su valor por defecto es
  \`NULL\`.

## Value

Un \`data.frame\` con el cálculo de la incidencia para todos los
departamentos de Colombia o los municipios de un departamento por sexo.

## Examples

``` r
# \donttest{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
# Cálculo de la incidencia con proyecciones poblacionales por sexo y
# departamento
data_filtrada <- geo_filtro(
  data_event = data_limpia,
  dpto = "05"
)
data_agrupada <- agrupar_sex(data_filtrada)
if (interactive()) {
  calcular_incidencia_sex(
    data_agrupada = data_agrupada,
    dpto = "05",
    year = 2020,
    cache = TRUE
  )
}
#' Cálculo de la incidencia con proyecciones poblacionales por sexo y
# municipio
data_filtrada <- geo_filtro(
  data_event = data_limpia,
  dpto = "05",
  mpio = "Medellin"
)
calcular_incidencia_sex(
  data_agrupada = data_agrupada,
  dpto = "05",
  mpio = "Medellin",
  ruta_dir = tempdir()
)
#> Las incidencias se calcularon con las proyecciones poblacionales DANE. Si usted cuenta con la poblacion a riesgo definida por el Ministerio de Salud para el 2020 puede hacer uso de ella, asignandola en el argumento data_incidencia de la funcion
#> $data_incidencia
#>   sexo casos cod_eve nombre_evento incidencia
#> 1    F     7     210        DENGUE       0.52
#> 2    M     5     210        DENGUE       0.42
#> 
#> $poblacion
#> [1] "proyecciones"
#> 
# }
```
