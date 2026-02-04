# Package index

## Importación de datos

- [`import_data_event()`](https://epiverse-trace.github.io/sivirep/reference/import_data_event.md)
  : Importar los datos de una enfermedad o evento por año desde los
  microdatos del SIVIGILA
- [`import_geo_cods()`](https://epiverse-trace.github.io/sivirep/reference/import_geo_cods.md)
  : Importar datos geográficos de Colombia
- [`import_pob_incidencia()`](https://epiverse-trace.github.io/sivirep/reference/import_pob_incidencia.md)
  : Importar la población para efectuar el cálculo de la incidencia
- [`import_pob_proyecciones()`](https://epiverse-trace.github.io/sivirep/reference/import_pob_proyecciones.md)
  : Importar las proyecciones DANE del año 2005 hasta el 2035
- [`import_pob_riesgo()`](https://epiverse-trace.github.io/sivirep/reference/import_pob_riesgo.md)
  : Importar la población a riesgo de un evento o enfermedad
- [`list_events()`](https://epiverse-trace.github.io/sivirep/reference/list_events.md)
  : Importar enfermedades y años disponibles para su descarga desde los
  microdatos del SIVIGILA

## Limpieza de datos

- [`limpiar_data_sivigila()`](https://epiverse-trace.github.io/sivirep/reference/limpiar_data_sivigila.md)
  : Limpiar datos de SIVIGILA
- [`limpiar_edad_event()`](https://epiverse-trace.github.io/sivirep/reference/limpiar_edad_event.md)
  : Limpiar las edades de los datos de una enfermedad o evento
- [`limpiar_encabezado()`](https://epiverse-trace.github.io/sivirep/reference/limpiar_encabezado.md)
  : Limpiar las etiquetas del encabezado
- [`limpiar_fecha_event()`](https://epiverse-trace.github.io/sivirep/reference/limpiar_fecha_event.md)
  : Limpiar fechas de los datos de una enfermedad o evento
- [`limpiar_val_atipic()`](https://epiverse-trace.github.io/sivirep/reference/limpiar_val_atipic.md)
  : Limpiar los valores atípicos de los datos
- [`estandarizar_geo_cods()`](https://epiverse-trace.github.io/sivirep/reference/estandarizar_geo_cods.md)
  : Estandarizar códigos geográficos de los datos de una enfermedad o
  evento
- [`convert_edad()`](https://epiverse-trace.github.io/sivirep/reference/convert_edad.md)
  : Convertir edad a años

## Distribuciones y agrupaciones por variable

- [`agrupar_area_geo()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_area_geo.md)
  : Agrupar por área geográfica
- [`agrupar_cols_casos()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_cols_casos.md)
  : Agrupar por columnas y casos
- [`agrupar_dpto()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_dpto.md)
  : Agrupar por departamento y casos
- [`agrupar_edad()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_edad.md)
  : Agrupar por edad y casos
- [`agrupar_edad_sex()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_edad_sex.md)
  : Agrupar por edades, sexo y casos
- [`agrupar_eventos()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_eventos.md)
  : Agrupar por tipo de enfermedad o evento
- [`agrupar_fecha_inisintomas()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_fecha_inisintomas.md)
  : Agrupar por fecha de inicio de síntomas y casos
- [`agrupar_mpio()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_mpio.md)
  : Agrupar por municipios y casos
- [`agrupar_per_etn()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_per_etn.md)
  : Agrupar por la pertenencia étnica
- [`agrupar_rango_edad()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_rango_edad.md)
  : Agrupar por rango de edad y casos
- [`agrupar_semanaepi()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_semanaepi.md)
  : Agrupar por semana epidemiológica y casos
- [`agrupar_sex()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_sex.md)
  : Agrupar por sexo y casos
- [`agrupar_sex_semanaepi()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_sex_semanaepi.md)
  : Agrupar por sexo, semana epidemiológica y casos
- [`agrupar_tipo_caso()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_tipo_caso.md)
  : Agrupar por la clasificación inicial del caso
- [`agrupar_top_area_geo()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_top_area_geo.md)
  : Agrupar por área geográfica a nivel departamental o municipal
- [`agrupar_years()`](https://epiverse-trace.github.io/sivirep/reference/agrupar_years.md)
  : Agrupar por años de una enfermedad o evento
- [`geo_filtro()`](https://epiverse-trace.github.io/sivirep/reference/geo_filtro.md)
  : Filtrar por departamentos y municipios

## Cálculo de la incidencia

- [`calcular_incidencia()`](https://epiverse-trace.github.io/sivirep/reference/calcular_incidencia.md)
  : Calcular incidencia
- [`calcular_incidencia_geo()`](https://epiverse-trace.github.io/sivirep/reference/calcular_incidencia_geo.md)
  : Calcular incidencia según distribución geográfica
- [`calcular_incidencia_sex()`](https://epiverse-trace.github.io/sivirep/reference/calcular_incidencia_sex.md)
  : Calcular incidencia por sexo

## Generación de gráficas, mapas y tablas

- [`plot_area_geo()`](https://epiverse-trace.github.io/sivirep/reference/plot_area_geo.md)
  : Generar gráfico de distribución de casos por área geográfica
- [`plot_dptos()`](https://epiverse-trace.github.io/sivirep/reference/plot_dptos.md)
  : Generar gráfico de distribución de casos por departamentos
- [`plot_edad()`](https://epiverse-trace.github.io/sivirep/reference/plot_edad.md)
  : Generar gráfico de distribución de casos por edad
- [`plot_edad_sex()`](https://epiverse-trace.github.io/sivirep/reference/plot_edad_sex.md)
  : Generar gráfico de distribución de casos por edad y sexo
- [`plot_fecha_inisintomas()`](https://epiverse-trace.github.io/sivirep/reference/plot_fecha_inisintomas.md)
  : Generar gráfico de distribución de casos por fecha de inicio de
  síntomas
- [`plot_map()`](https://epiverse-trace.github.io/sivirep/reference/plot_map.md)
  : Generar mapa
- [`plot_mpios()`](https://epiverse-trace.github.io/sivirep/reference/plot_mpios.md)
  : Generar gráfico de distribución de casos por municipios
- [`plot_per_etn()`](https://epiverse-trace.github.io/sivirep/reference/plot_per_etn.md)
  : Generar gráfico de distribución de casos por pertenencia étnica
- [`plot_sex()`](https://epiverse-trace.github.io/sivirep/reference/plot_sex.md)
  : Generar gráfico de distribución de casos por sexo
- [`plot_sex_semanaepi()`](https://epiverse-trace.github.io/sivirep/reference/plot_sex_semanaepi.md)
  : Generar gráfico de distribución de casos por sexo y semana
  epidemiológica
- [`plot_tabla_incidencia_geo()`](https://epiverse-trace.github.io/sivirep/reference/plot_tabla_incidencia_geo.md)
  : Generar tabla con la incidencia
- [`plot_tabla_incidencia_sex()`](https://epiverse-trace.github.io/sivirep/reference/plot_tabla_incidencia_sex.md)
  : Generar tabla con la incidencia por sexo
- [`plot_tabla_tipos_event()`](https://epiverse-trace.github.io/sivirep/reference/plot_tabla_tipos_event.md)
  : Generar tabla con la distribución de casos por tipo de enfermedad o
  evento
- [`plot_tipo_caso()`](https://epiverse-trace.github.io/sivirep/reference/plot_tipo_caso.md)
  : Generar gráfico de distribución de casos por la clasificación
  inicial del caso
- [`plot_tipo_caso_years()`](https://epiverse-trace.github.io/sivirep/reference/plot_tipo_caso_years.md)
  : Generar gráfico de distribución de casos por la clasificación
  inicial del caso y los años seleccionados
- [`plot_top_area_geo()`](https://epiverse-trace.github.io/sivirep/reference/plot_top_area_geo.md)
  : Generar gráfico de distribución de casos por área geográfica a nivel
  departamental o municipal
- [`plot_years()`](https://epiverse-trace.github.io/sivirep/reference/plot_years.md)
  : Generar gráfico de distribución de casos por año

## Utilidades

- [`obtener_cond_inciden_event()`](https://epiverse-trace.github.io/sivirep/reference/obtener_cond_inciden_event.md)
  : Obtener las condiciones para calcular la incidencia de una
  enfermedad o evento
- [`obtener_dptos()`](https://epiverse-trace.github.io/sivirep/reference/obtener_dptos.md)
  : Obtener departamentos de Colombia
- [`obtener_fila_mas_casos()`](https://epiverse-trace.github.io/sivirep/reference/obtener_fila_mas_casos.md)
  : Obtener la fila con mayor número de casos
- [`obtener_info_depts()`](https://epiverse-trace.github.io/sivirep/reference/obtener_info_depts.md)
  : Obtener información geográfica de los datos de la enfermedad o
  evento
- [`obtener_meses_mas_casos()`](https://epiverse-trace.github.io/sivirep/reference/obtener_meses_mas_casos.md)
  : Obtener los meses con mayor número de casos
- [`obtener_nombre_dpto()`](https://epiverse-trace.github.io/sivirep/reference/obtener_nombre_dpto.md)
  : Obtener el nombre de un departamento de Colombia
- [`obtener_nombre_mpio()`](https://epiverse-trace.github.io/sivirep/reference/obtener_nombre_mpio.md)
  : Obtener el nombre de un municipio de Colombia
- [`obtener_text_sex()`](https://epiverse-trace.github.io/sivirep/reference/obtener_text_sex.md)
  : Obtener el párrafo de la distribución de casos por sexo
- [`obtener_tip_ocurren_geo()`](https://epiverse-trace.github.io/sivirep/reference/obtener_tip_ocurren_geo.md)
  : Obtener columnas de ocurrencia geográfica de los datos de la
  enfermedad o evento
- [`obtener_val_config()`](https://epiverse-trace.github.io/sivirep/reference/obtener_val_config.md)
  : Obtener valor del archivo de configuración

## Datos del paquete

- [`dengue2020`](https://epiverse-trace.github.io/sivirep/reference/dengue2020.md)
  : Datos Dengue 2020 del SIVIGILA en sivirep
- [`divipoladata`](https://epiverse-trace.github.io/sivirep/reference/divipoladata.md)
  : Códigos e información geografica del DIVIPOLA en sivirep
