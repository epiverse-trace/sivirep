data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_estandar <- estandarizar_geo_cods(data_limpia)

test_that("`mapa` maneja errores correctamente", {
  expect_error(
    plot_map(data_agrupada = list(a = 1, b = 2)),
    "El parametro data_agrupada debe ser un data.frame"
  )
  expect_error(
    plot_map(data_agrupada = data_limpia, fuente_data = 1),
    "El parametro fuente_data debe ser un cadena de caracteres"
  )
})

test_that("`mapa_colombia` funciona correctamente", {
  data_espacial <- agrupar_dpto(data_event = data_estandar)

  expect_s3_class(data_espacial, "data.frame")
  expect_true("cod_eve" %in% names(data_espacial))
  expect_true("nombre_evento" %in% names(data_espacial))
  expect_true("ano" %in% names(data_espacial))
  expect_true("cod_dpto_o" %in% names(data_espacial))
  expect_true("departamento_ocurrencia" %in% names(data_espacial))
  expect_true("casos" %in% names(data_espacial))

  map <- plot_map(
    data_agrupada = data_espacial,
    col_distribucion = "casos"
  )

  expect_s3_class(map, "ggplot")
})

test_that("`mapa_dpto` funciona correctamente", {
  data_filtrada_dpto <- geo_filtro(
    data_event = data_estandar,
    dpto = "Cundinamarca"
  )
  data_espacial_dpto <- agrupar_mpio(data_event = data_filtrada_dpto)

  expect_s3_class(data_espacial_dpto, "data.frame")
  expect_true("cod_eve" %in% names(data_espacial_dpto))
  expect_true("nombre_evento" %in% names(data_espacial_dpto))
  expect_true("ano" %in% names(data_espacial_dpto))
  expect_true("cod_dpto_o" %in% names(data_espacial_dpto))
  expect_true("departamento_ocurrencia" %in% names(data_espacial_dpto))
  expect_true("cod_mun_o" %in% names(data_espacial_dpto))
  expect_true("municipio_ocurrencia" %in% names(data_espacial_dpto))
  expect_true("casos" %in% names(data_espacial_dpto))

  map <- plot_map(
    data_agrupada = data_espacial_dpto,
    col_codigos = "cod_mun_o",
    col_distribucion = "casos"
  )

  expect_s3_class(map, "ggplot")
})

test_that("`mapa_mpio` funciona correctamente", {
  data_filtrada_mpio <- geo_filtro(
    data_event = data_estandar,
    dpto = "Antioquia",
    mpio = "Medellin"
  )
  data_espacial_mpio <- agrupar_mpio(data_event = data_filtrada_mpio)

  expect_s3_class(data_espacial_mpio, "data.frame")
  expect_true("cod_eve" %in% names(data_espacial_mpio))
  expect_true("nombre_evento" %in% names(data_espacial_mpio))
  expect_true("ano" %in% names(data_espacial_mpio))
  expect_true("cod_dpto_o" %in% names(data_espacial_mpio))
  expect_true("departamento_ocurrencia" %in% names(data_espacial_mpio))
  expect_true("cod_mun_o" %in% names(data_espacial_mpio))
  expect_true("municipio_ocurrencia" %in% names(data_espacial_mpio))
  expect_true("casos" %in% names(data_espacial_mpio))

  map <- plot_map(
    data_agrupada = data_espacial_mpio,
    col_distribucion = "casos",
    dpto = "Antioquia",
    mpio = "Envigado"
  )

  expect_s3_class(map, "ggplot")
})

test_that("`mapa_indicidencia_colombia` funciona correctamente", {
  data_agrupada <- agrupar_dpto(data_event = data_estandar)
  incidencia_dptos <- calcular_incidencia_geo(
    data_agrupada =
      data_agrupada
  )
  expect_s3_class(incidencia_dptos, "data.frame")
  expect_true("cod_eve" %in% names(incidencia_dptos))
  expect_true("nombre_evento" %in% names(incidencia_dptos))
  expect_true("cod_dpto_o" %in% names(incidencia_dptos))
  expect_true("departamento_ocurrencia" %in% names(incidencia_dptos))
  expect_true("casos" %in% names(incidencia_dptos))
  expect_true("incidencia" %in% names(incidencia_dptos))

  map <- plot_map(data_agrupada = incidencia_dptos)
  expect_s3_class(map, "ggplot")
})

test_that("`mapa_indicidencia_dpto` funciona correctamente", {
  data_agrupada <- agrupar_mpio(
    data_event = data_estandar,
    dpto = "Antioquia"
  )
  incidencia_dpto <- calcular_incidencia_geo(
    data_agrupada =
      data_agrupada
  )
  expect_s3_class(incidencia_dpto, "data.frame")
  expect_true("nombre_evento" %in% names(incidencia_dpto))
  expect_true("cod_dpto_o" %in% names(incidencia_dpto))
  expect_true("departamento_ocurrencia" %in% names(incidencia_dpto))
  expect_true("cod_mun_o" %in% names(incidencia_dpto))
  expect_true("municipio_ocurrencia" %in% names(incidencia_dpto))
  expect_true("casos" %in% names(incidencia_dpto))
  expect_true("incidencia" %in% names(incidencia_dpto))

  map <- plot_map(data_agrupada = incidencia_dpto)
  expect_s3_class(map, "ggplot")
})

test_that("`mapa_indicidencia_mpio` funciona correctamente", {
  data_filtrada_mpio <- geo_filtro(
    data_event = data_estandar,
    dpto = "Antioquia",
    mpio = "Medellin"
  )
  data_agrupada <- agrupar_mpio(
    data_event = data_filtrada_mpio,
    dpto = "Antioquia"
  )
  incidencia_mpio <- calcular_incidencia_geo(
    data_agrupada =
      data_agrupada
  )
  expect_s3_class(incidencia_mpio, "data.frame")
  expect_true("nombre_evento" %in% names(incidencia_mpio))
  expect_true("cod_dpto_o" %in% names(incidencia_mpio))
  expect_true("departamento_ocurrencia" %in% names(incidencia_mpio))
  expect_true("cod_mun_o" %in% names(incidencia_mpio))
  expect_true("municipio_ocurrencia" %in% names(incidencia_mpio))
  expect_true("casos" %in% names(incidencia_mpio))
  expect_true("incidencia" %in% names(incidencia_mpio))

  map <- plot_map(data_agrupada = incidencia_mpio)
  expect_s3_class(map, "ggplot")
})
