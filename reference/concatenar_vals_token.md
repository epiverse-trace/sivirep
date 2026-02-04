# Concatenar valores con separador o token

Función que concatena valores con un separador o token específico.

## Usage

``` r
concatenar_vals_token(
  vals,
  longitud = 3,
  princ_token = ", ",
  final_token = "y"
)
```

## Arguments

- vals:

  Un \`array\` (arreglo) de character (cadena de caracteres) que
  contiene los valores que se desean concatenar.

- longitud:

  Un \`numeric\` (numérico) que contiene la longitud de los valores que
  se desean concatenar; su valor por defecto es \`3\`.

- princ_token:

  Un \`character\` (cadena de caracteres) que contiene el separador o
  token principal; su valor por defecto es \`", "\`.

- final_token:

  Un \`character\` (cadena de caracteres) que contiene el separador o
  token final; su valor por defecto es \`"y "\`.

## Value

Un \`character\` (cadena de caracteres) con el valor final concatenado.
