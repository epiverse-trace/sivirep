# Obtener valor del archivo de configuración

Función que obtiene el valor de una llave del archivo de configuración.

## Usage

``` r
obtener_val_config(llave)
```

## Arguments

- llave:

  Un \`character\` (cadena de caracteres) con el nombre de la llave que
  se encuentra en el archivo de configuración del paquete.

## Value

Un \`character\` (cadena de caracteres) con el valor de la llave del
archivo de configuración del paquete.

## Examples

``` r
obtener_val_config("request_timeout")
#> [1] 2000000
```
