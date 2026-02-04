# Obtener la ruta de descarga de una enfermedad por un año específico

Función que obtiene la ruta o URL de la API del SIVIGILA para descargar
los datos de una enfermedad o evento para un año específico.

## Usage

``` r
obtener_ruta_data_event_year(nombre_event, year)
```

## Arguments

- nombre_event:

  Un \`character\` (cadena de caracteres) con el nombre de la enfermedad
  o evento.

- year:

  Un \`numeric\` (numérico) con el año o años deseado(s) para la
  descarga de los datos.

## Value

Un \`character\` (cadena de caracteres) con la ruta o URL para descargar
los datos de una enfermedad o evento por un año específico de la fuente
SIVIGILA.
