# Plantilla reporte automatizado

Antes de generar el reporte automatizado, revisa la lista de
enfermedades disponibles para hacer un reporte con `sivirep` utilizando
el comando:

``` r
list_events()
```

También, puedes ver el listado de enfermedades en la página [Recursos
sivirep.](https://epiverse-trace.github.io/sivirep/articles/resources.html#enfermedades-y-a%C3%B1os-disponibles-para-su-descarga)

Actualmente, `sivirep` provee una [plantilla de reporte en R
Markdown](https://rstudio.github.io/rstudio-extensions/rmarkdown_templates.html)
llamada `Reporte Evento {sivirep}`, la cual contiene 12 secciones y
recibe los siguientes parámetros de entrada:

- Nombre de la enfermedad
- Año
- País
- Nombre de departamento (opcional)
- Nombre del municipio (opcional)

Con esta plantilla podrás generar reportes a nivel nacional,
departamental o municipal en Colombia y descargar la información sobre
la enfermedad o evento de interés directamente de los microdatos del
[SIVIGILA](https://www.ins.gov.co/Direcciones/Vigilancia/Paginas/SIVIGILA.aspx)
(Sistema Nacional de Vigilancia en Salud Pública de Colombia), según los
valores que ingreses en estos parámetros.

Para hacer uso de la plantilla del reporte primero debes importar el
paquete después de finalizada su
[instalación](https://epiverse-trace.github.io/sivirep/articles/sivirep.html#instalaci%C3%B3n)
con el comando:

``` r
library(sivirep)
```

y seguir los siguientes pasos:

> 🎥 **Tip**: Si deseas ver el paso a paso directamente en RStudio, te
> recomendamos ver el video [¿Cómo generar un reporte con
> sivirep?](https://youtu.be/wsgXQKEeg8I)

1.  En RStudio dirígete a la parte superior izquierda de la ventana, haz
    clic en el icono de
    ![](https://raw.githubusercontent.com/epiverse-trace/sivirep/main/inst/images/archivo_nuevo.png)
    y selecciona la opción de *‘R Markdown’*:

![](https://raw.githubusercontent.com/epiverse-trace/sivirep/main/inst/images/opcion_rmarkdown.png)

2.  Selecciona la opción del panel izquierdo: *‘From Template’*, después
    el template del reporte llamado: `Reporte Evento {sivirep}`, indica
    el nombre que deseas para el reporte (i.e. ReporteDengue), la
    ubicación donde deseas guardarlo y presiona *‘Ok’*.

![](https://raw.githubusercontent.com/epiverse-trace/sivirep/main/inst/images/template_reporte_basico.png)

3.  Presiona el botón *’Knit*’, el cual puedes encontrar en la parte
    superior de RStudio, despliega las opciones y selecciona *’Knit with
    parameters*’.

![](https://raw.githubusercontent.com/epiverse-trace/sivirep/main/inst/images/knit_parameters.png)

4.  Selecciona el nombre de la enfermedad, el año, el país, el
    departamento (opcional), el municipio (opcional) y las secciones que
    deseas visualizar en el reporte y haz clic en el botón *‘Knit’*.
    Esta acción descargará los datos deseados y también proporcionará la
    plantilla en un archivo R Markdown (.Rmd).

![](https://raw.githubusercontent.com/epiverse-trace/sivirep/main/inst/images/template_parametros.png)

4.  Espera unos segundos mientras el informe se genera en un archivo PDF
    o HTML.

5.  Puedes agregar, editar, eliminar y personalizar las secciones del
    reporte en el archivo R Markdown generado anteriormente.

![](https://raw.githubusercontent.com/epiverse-trace/sivirep/main/inst/images/edit_rmarkdown.png)

> ⚠️ Si deseas generar el reporte en formato PDF debes instalar LateX.
> Puedes instalarlo siguiendo las instrucciones que se encuentran en [R
> Markdown
> Cookbook.](https://bookdown.org/yihui/rmarkdown-cookbook/install-latex.html)

Para obtener más detalles sobre plantillas y reportes genéricos de R
Markdown, por favor consulta [R Markdown
templates](https://rstudio.github.io/rstudio-extensions/rmarkdown_templates.html).
