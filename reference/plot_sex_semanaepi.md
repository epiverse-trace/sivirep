# Generar gráfico de distribución de casos por sexo y semana epidemiológica

Función que genera un gráfico de distribución de casos por sexo y semana
epidemiológica.

## Usage

``` r
plot_sex_semanaepi(
  data_agrupada,
  col_sex = "sexo",
  col_semanaepi = "semana",
  fuente_data = NULL
)
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento
  agrupados.

- col_sex:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene el sexo en los datos agrupados de la enfermedad o evento;
  su valor por defecto es \`"sexo"\`.

- col_semanaepi:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene las semanas epidemiológicas en los datos agrupados de la
  enfermedad o evento; su valor por defecto es \`"semana"\`.

- fuente_data:

  Un \`character\` (cadena de caracteres) que contiene la leyenda o
  fuente de información de los datos; su valor por defecto es \`NULL\`.

## Value

Un \`plot\` o gráfico de distribución de casos por sexo y semana
epidemiológica.

## Examples

``` r
# \donttest{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_agrupada <- agrupar_sex_semanaepi(data_event = data_limpia)
plot_sex_semanaepi(
  data_agrupada = data_agrupada,
  col_sex = "sexo",
  col_semanaepi = "semana"
)

# }
```
