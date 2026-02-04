# Obtener la población para efectuar el cálculo de la incidencia

Función que obtiene la población a riesgo de un evento o enfermedad o
las proyecciones poblacionales DANE desde el año 2005 hasta el 2035. Si
no hay población a riesgo disponible del evento o enfermedad para el año
seleccionado, se obtendrán las proyecciones poblacionales DANE y se
mostrarán mensajes al usuario dependendiendo del tipo de población
obtenida.

## Usage

``` r
obtener_pob_incidencia(
  data_incidencia = NULL,
  poblacion,
  event,
  year,
  ruta_dir = NULL,
  cache = FALSE
)
```

## Arguments

- data_incidencia:

  Un \`data.frame\` que contiene la población a riesgo o las
  proyecciones poblaciones DANE. Si este parámetro está vacío importará
  la población a riesgo o las proyecciones dependiendo de la
  disponibilidad de la información y las condiciones del evento o
  enfermedad; su valor por defecto es \`NULL\`.

- poblacion:

  Un \`character\` (cadena de caracteres) con el tipo de población que
  se desea obtener. Puede ser \`"riesgo"\` para la población a riesgo
  del evento o \`"proyecciones"\` para las proyecciones poblacionales
  DANE.

- event:

  Un \`character\` (cadena de caracteres) o un \`numeric\` (numérico)
  con el nombre o código de la enfermedad o evento. Es obligatorio para
  obtener la población a riesgo.

- year:

  Un \`numeric\` (numérico) con el año deseado de la población a riesgo.
  Es obligatorio para obtener la población a riesgo.

- ruta_dir:

  Un \`character\` (cadena de caracteres) que especifica la ruta del
  directorio donde se almacenarán la población a riesgo o las
  proyecciones poblacionales DANE. Su valor por defecto es \`NULL\`.

- cache:

  Un \`logical\` (\`TRUE\` o \`FALSE\`) que indica si la población a
  riesgo o las proyecciones poblacionales DANE descargadas deben ser
  almacenados en caché. Su valor por defecto es \`FALSE\`.

## Value

Un \`data.frame\` con la población a riesgo o las proyecciones
poblacionales DANE.
