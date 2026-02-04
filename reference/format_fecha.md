# Formatear fechas

Función que da un formato específico a una fecha.

## Usage

``` r
format_fecha(data_event, format_fecha = "%Y-%m-%d", nomb_cols = NULL)
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de un evento o enfermedad.

- format_fecha:

  Un \`character\` (cadena de caracteres) que contiene el formato
  deseado de la fecha; su valor por defecto es \`"%Y-%m-%d"\`.

- nomb_cols:

  Un \`character\` (cadena de caracteres) o \`array\` de \`character\`
  que contiene los nombres de la columnas a formatear en los datos de
  una enfermedad o evento; su valor por defecto es \`NULL\`.

## Value

Un \`data.frame\` con los datos con las fechas formateadas.
