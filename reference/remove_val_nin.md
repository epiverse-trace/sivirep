# Eliminar valores NIN (NA, Infinito, NaN)

Función que elimina filas si los valores de la columna seleccionada
incluyen NA, Infinito o NaN.

## Usage

``` r
remove_val_nin(data_event, nomb_col)
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de una enfermedad o evento.

- nomb_col:

  Un \`character\` (cadena de caracteres) que contiene el nombre de la
  columna a evaluar en los datos de una enfermedad o evento.

## Value

Un \`data.frame\` con los datos limpios sin valores NA, Infinito o NaN.
