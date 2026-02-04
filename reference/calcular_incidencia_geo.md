# Calcular incidencia según distribución geográfica

Función que calcula la incidencia de una enfermedad o evento para todos
los departamentos de Colombia o los municipios de un departamento.

## Usage

``` r
calcular_incidencia_geo(
  data_incidencia = NULL,
  cache = FALSE,
  ruta_dir = NULL,
  data_agrupada,
  poblacion = NULL,
  year = NULL
)
```

## Arguments

- data_incidencia:

  Un \`data.frame\` que contiene las proyecciones poblacionales del
  DANE; su valor por defecto es \`NULL\`.

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

## Value

Un \`data.frame\` con el cálculo de la incidencia para todos los
departamentos de Colombia o los municipios de un departamento.

## Examples

``` r
# \donttest{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
data_agrupada_mpios <- agrupar_mpio(data_limpia, dpto = "Antioquia")
# Cálculo de la incidencia con población a riesgo por departamento
if (interactive()) {
  calcular_incidencia_geo(
    poblacion = "riesgo",
    data_agrupada = data_agrupada_mpios,
    year = 2020,
    cache = TRUE
  )
}
data_agrupada_dptos <- agrupar_dpto(data_limpia)
# Cálculo de la incidencia con proyecciones poblacionales para Colombia
calcular_incidencia_geo(
  poblacion = "proyecciones",
  data_agrupada = data_agrupada_dptos,
  year = 2020,
  ruta_dir = tempdir()
)
#> Las incidencias que va a generar idealmente deberian estar calculadas por poblacion a riesgo. Para esto, puede usar el argumento poblacion = 'riesgo'
#> $data_incidencia
#>    cod_dpto_o departamento_ocurrencia casos nombre_evento cod_eve incidencia
#> 1          01                EXTERIOR     1        DENGUE     210       0.00
#> 2          05               ANTIOQUIA    12        DENGUE     210       0.18
#> 3          08               ATLANTICO     2        DENGUE     210       0.07
#> 4          13                 BOLIVAR     1        DENGUE     210       0.05
#> 5          15                  BOYACA     1        DENGUE     210       0.08
#> 6          17                  CALDAS     1        DENGUE     210       0.10
#> 7          18                 CAQUETA     1        DENGUE     210       0.24
#> 8          19                   CAUCA     1        DENGUE     210       0.07
#> 9          20                   CESAR     1        DENGUE     210       0.08
#> 10         23                 CORDOBA     1        DENGUE     210       0.05
#> 11         25            CUNDINAMARCA     1        DENGUE     210       0.03
#> 12         27                   CHOCO     1        DENGUE     210       0.18
#> 13         41                   HUILA     1        DENGUE     210       0.09
#> 14         44                 GUAJIRA     1        DENGUE     210       0.10
#> 15         47               MAGDALENA     1        DENGUE     210       0.07
#> 16         50                    META     1        DENGUE     210       0.09
#> 17         52                  NARIÑO     1        DENGUE     210       0.06
#> 18         54         NORTE SANTANDER     1        DENGUE     210       0.06
#> 19         63                 QUINDIO     1        DENGUE     210       0.18
#> 20         66               RISARALDA     2        DENGUE     210       0.21
#> 21         68               SANTANDER     1        DENGUE     210       0.04
#> 22         70                   SUCRE     1        DENGUE     210       0.11
#> 23         73                  TOLIMA     2        DENGUE     210       0.15
#> 24         76                   VALLE     1        DENGUE     210       0.02
#> 25         81                  ARAUCA     2        DENGUE     210       0.69
#> 26         85                CASANARE     1        DENGUE     210       0.23
#> 27         86                PUTUMAYO     1        DENGUE     210       0.27
#> 28         88              SAN ANDRES     1        DENGUE     210       1.62
#> 29         91                AMAZONAS     1        DENGUE     210       1.24
#> 30         94                 GUAINIA     1        DENGUE     210       1.92
#> 31         97                  VAUPES     2        DENGUE     210       4.63
#> 
#> $poblacion
#> [1] "proyecciones"
#> 
# }
```
