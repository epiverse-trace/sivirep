# Obtener la configuración del mapa

Función que obtiene el departamento, municipio y el poligono requeridos
para generar el mapa.

## Usage

``` r
obtener_config_map(data_agrupada, dpto, mpio, geo_ocurrencia, shp)
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad agrupados
  por departamento y número de casos.

- dpto:

  Un \`character\` (cadena de caracteres) que contiene el nombre del
  departamento.

- mpio:

  Un \`character\` (cadena de caracteres) que contiene el nombre del
  municipio.

- geo_ocurrencia:

  Un \`data.frame\` con las columnas de ocurrencia geográfica de los
  datos de la enfermedad o evento.

- shp:

  Objeto que contiene el\`Shapefile\` del mapa.

## Value

Una \`named list\` (lista nombrada) que contiene el departamento,
municipio y el polígono para generar el mapa con los siguientes nombres
para cada uno de estos elementos: \`dpto\`, \`mpio\` y \`poligono\`.
