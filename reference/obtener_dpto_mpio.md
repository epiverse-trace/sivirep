# Obtener código de un departamento y municipio

Función que obtiene los códigos geográficos de un departamento y
municipio dadas unas condiciones.

## Usage

``` r
obtener_dpto_mpio(data_agrupada, nomb_cols, dpto = NULL, mpio = NULL)
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad agrupados
  por departamento o municipio y número de casos.

- nomb_cols:

  Un \`character\` (cadena de caracteres) o \`array\` (arreglo) de
  \`character\` que contiene el nombre de la(s) columna(s) con la
  información de los departamentos y municipios en los datos agrupados
  de la enfermedad o evento.

- dpto:

  Un \`character\` (cadena de caracteres) o \`numeric\` (numérico) que
  contiene el código o nombre del departamento; su valor por defecto es
  \`NULL\`.

- mpio:

  Un \`character\` (cadena de caracteres) o \`numeric\` (numérico) que
  contiene el código o nombre del municipio; su valor por defecto es
  \`NULL\`.

## Value

Una \`list\` (lista) con el departamento y municipio con la siguiente
estructura \`list(dpto = "05", mpio = "05001")\`.
