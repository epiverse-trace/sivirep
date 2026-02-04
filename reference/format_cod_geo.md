# Formatear código geográfico

Función que da el formato deseado a un código geográfico.

## Usage

``` r
format_cod_geo(cod_geo, etiqueta, digitos, tam)
```

## Arguments

- cod_geo:

  Un \`numeric\` (numérico) o \`character\` (cadena de caracteres) que
  contiene el código geográfico.

- etiqueta:

  Un \`character\` (cadena de caracteres) con el nombre de la etiqueta
  de la validación relacionada a la longitud máxima del código
  geográfico; se refiere al tipo de división geográfica ("municipio",
  "departamento").

- digitos:

  Un \`numeric\` (numérico) que contiene el número de digitos que debe
  tener individualmente el código geográfico.

- tam:

  Un \`numeric\` (numérico) que contiende el tamaño o la longitud máxima
  que debe tener el código geográfico.

## Value

Un \`character\` (cadena de caracteres) con el código geográfico
formateado.
