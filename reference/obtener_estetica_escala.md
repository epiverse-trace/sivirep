# Obtener la estética de una escala para un gráfico de sivirep

Función que genera la estética de una escala para un gráfico de sivirep.

## Usage

``` r
obtener_estetica_escala(
  escala = 0,
  nombre,
  etiquetas = NULL,
  ajustar_texto = FALSE
)
```

## Arguments

- escala:

  Un \`numeric\` (numérico) que indica la cantidad de valores que
  contiene la escala.

- nombre:

  Un \`character\` (cadena de caracteres) que contiene el nombre de la
  escala.

- etiquetas:

  Un \`character\` (cadena de caracteres) que contiene las etiquetas de
  la escala.

## Value

Un objeto \`scale_fill_manual\` de ggplot2.
