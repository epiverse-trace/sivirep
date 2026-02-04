# Calcular incidencia

Función que calcula la incidencia de una enfermedad o evento para todo
Colombia, un departamento o un municipio.

## Usage

``` r
calcular_incidencia(
  data_incidencia = NULL,
  cache = FALSE,
  ruta_dir = NULL,
  data_agrupada,
  poblacion = NULL,
  year = NULL,
  dpto = NULL,
  mpio = NULL,
  sex = NULL
)
```

## Arguments

- data_incidencia:

  Un \`data.frame\` que contiene la población a riesgo o las
  proyecciones poblaciones DANE. Si este parámetro está vacío, se
  importará la población a riesgo o las proyecciones, dependiendo de la
  disponibilidad de la información; su valor por defecto es \`NULL\`.

- cache:

  Un \`logical\` (\`TRUE\` o \`FALSE\`) que indica si la población a
  riesgo o las proyecciones poblacionales DANE descargadas deben ser
  almacenados en caché. Su valor por defecto es \`FALSE\`.

- ruta_dir:

  Un \`character\` (cadena de caracteres) que especifica la ruta del
  directorio donde se almacenarán la población a riesgo o las
  proyecciones poblacionales DANE. Su valor por defecto es \`NULL\`.

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad agrupados
  por departamento o municipio y número de casos.

- poblacion:

  Un \`character\` (cadena de caracteres) con el tipo de población para
  calcular la incidencia. Puede ser \`"riesgo"\` para la población a
  riesgo o \`"proyecciones"\` para las proyecciones poblacionales DANE;
  su valor por defecto es \`NULL\`.

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

- sex:

  Un \`character\` (cadena de caracteres) que especifica el sexo:
  \`"F"\` para Femenino y \`"M"\` para Masculino; su valor por defecto
  es \`NULL\`.

## Value

Un \`numeric\` con el cálculo de la incidencia para todo Colombia, un
departamento, municipio o sexo especifico.

## Examples

``` r
# \donttest{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
# Cálculo de la incidencia con proyecciones poblacionales por departamento
data_agrupada_mpios <- agrupar_mpio(data_limpia, dpto = "Antioquia")
if (interactive()) {
  calcular_incidencia(
    data_agrupada = data_agrupada_mpios,
    poblacion = "proyecciones",
    dpto = "05",
    year = 2020,
    cache = TRUE
  )
}
# Cálculo de la incidencia con proyecciones poblacionales por municipio
calcular_incidencia(
  data_agrupada = data_agrupada_mpios,
  poblacion = "proyecciones",
  dpto = "Antioquia",
  mpio = "05001",
  year = 2020,
  ruta_dir = tempdir()
)
#> Las incidencias que va a generar idealmente deberian estar calculadas por poblacion a riesgo. Para esto, puede usar el argumento poblacion = 'riesgo'
#> $incidencia
#> [1] 0.16
#> 
#> $poblacion
#> [1] "proyecciones"
#> 
# Cálculo de la incidencia con población a riesgo para Colombia
data_agrupada_dptos <- agrupar_dpto(data_limpia)
calcular_incidencia(
  poblacion = "riesgo",
  data_agrupada = data_agrupada_dptos,
  year = 2020,
  ruta_dir = tempdir()
)
#> Las incidencias se calcularon con la poblacion a riesgo definida por el Ministerio de Salud para el 2020
#> $incidencia
#> [1] 0.14
#> 
#> $poblacion
#> [1] "riesgo"
#> 
# }
```
