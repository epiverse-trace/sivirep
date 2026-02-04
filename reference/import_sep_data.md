# Importar datos con un separador específico

Función que importa e identifica el separador que tiene los datos para
tabularlos.

## Usage

``` r
import_sep_data(ruta_data = NULL, ruta_dir = NULL, cache = FALSE)
```

## Arguments

- ruta_data:

  Un \`character\` (cadena de caracteres) que contiene la URL de los
  datos de SIVIGILA.

- ruta_dir:

  Un \`character\` (cadena de caracteres) que contiene la ruta del
  directorio donde se almacenarán los datos del evento o enfermedad. Su
  valor por defecto es \`NULL\`.

- cache:

  Un \`logical\` (\`TRUE\` o \`FALSE\`) que indica si los datos
  descargados deben ser almacenados en caché. Su valor por defecto es
  \`FALSE\`.

## Value

Un \`data.frame\` con los datos tabulados.
