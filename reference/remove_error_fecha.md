# Eliminar fechas mayores que el valor de comparación

Función que elimina fechas mayores que el valor de comparación.

## Usage

``` r
remove_error_fecha(data_event, col_ini = "ini_sin", col_comp = "fec_hos")
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de una enfermedad o evento.

- col_ini:

  Un \`character\` (cadena de caracteres) que contiene el nombre de la
  columna de la fecha inicial en los datos de una enfermedad o evento;
  su valor por defecto es \`"ini_sin"\`.

- col_comp:

  Un \`character\` (cadena de caracteres) que contiene el nombre de la
  columna de la fecha de comparación en los datos de una enfermedad o
  evento; su valor por defecto es \`"fec_hos\`".

## Value

Un \`data.frame\` con los datos sin las fechas mayores que el valor de
comparación.
