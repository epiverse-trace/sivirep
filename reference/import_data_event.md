# Importar los datos de una enfermedad o evento por año desde los microdatos del SIVIGILA

Función que importa los datos de una enfermedad o evento por año desde
los microdatos del SIVIGILA.

Esta función requiere acceso a Internet.

## Usage

``` r
import_data_event(nombre_event, years, ruta_dir = NULL, cache = FALSE)
```

## Arguments

- nombre_event:

  Un \`character\` (cadena de caracteres) con el nombre de la enfermedad
  o evento.

- years:

  Un \`numeric\` (numérico) con el año o años deseado(s) para la
  descarga de los datos.

- ruta_dir:

  Un \`character\` (cadena de caracteres) que contiene la ruta del
  directorio donde se almacenarán los datos del evento o enfermedad. Su
  valor por defecto es \`NULL\`.

- cache:

  Un \`logical\` (\`TRUE\` o \`FALSE\`) que indica si los datos
  descargados deben ser almacenados en caché. Su valor por defecto es
  \`FALSE\`.

## Value

Un \`data.frame\` con los datos del año de la enfermedad o evento
seleccionado desde los microdatos del SIVIGILA.

## Details

Para ver la lista de valores aceptados para el parámetro
\`nombre_event\`, ejecuta la función \[\`list_events()\`\]. Se
recomienda consultar esta lista para evitar errores por nombres de
enfermedades o eventos no válidos.

## Examples

``` r
# \donttest{
if (interactive()) {
import_data_event(nombre_event = "DENGUE",
                  years = 2020,
                  cache = TRUE)
import_data_event(nombre_event = "CHAGAS",
                  years = c(2019, 2020),
                  ruta_dir = tempdir())
import_data_event(nombre_event = "CHAGAS",
                  years = seq(2018, 2020),
                  cache = TRUE)
 }
# }
```
