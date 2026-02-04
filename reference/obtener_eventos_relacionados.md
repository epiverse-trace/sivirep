# Obtener los eventos relacionados

Función que obtiene los eventos relacionados o tipos de un evento
principal.

## Usage

``` r
obtener_eventos_relacionados(nombre_event, years, eventos_disponibles)
```

## Arguments

- nombre_event:

  Un \`character\` (cadena de caracteres) con el nombre de la enfermedad
  o evento.

- years:

  Un \`numeric\` (numérico) con el año o años deseados para la descarga
  de los datos.

## Value

Un \`array\` con los eventos relacionados por año desde los microdatos
de SIVIGILA.
