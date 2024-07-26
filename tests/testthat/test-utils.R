data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
set.seed(123)

test_that("`obtener_meses_mas_casos` funciona correctamente", {
  casos_inisintomas <- agrupar_fecha_inisintomas(data_limpia)
  result_meses <- obtener_meses_mas_casos(
    data_event = casos_inisintomas,
    col_fechas = "ini_sin",
    col_casos = "casos",
    top = 3,
    concat_vals = TRUE
  )

  expect_equal(result_meses, "enero")
})

test_that("`obtener_meses_mas_casos` maneja errores correctamente", {
  expect_error(
    obtener_meses_mas_casos(
      data_event = list(a = 1, b = 2),
      col_fechas = "ini_sin"
    ),
    "El parametro data_event debe ser un data.frame"
  )

  expect_error(
    obtener_meses_mas_casos(
      data_event = data_limpia,
      col_fechas = 1
    ),
    "El parametro col_fechas debe ser una cadena de caracteres"
  )
})

test_that("`obtener_fila_mas_casos` funciona correctamente", {
  casos_sex <- agrupar_sex(
    data_event = data_limpia,
    porcentaje = TRUE
  )
  result_fila <- obtener_fila_mas_casos(data_event = casos_sex)
  expect_s3_class(result_fila, "data.frame")

  expect_equal(result_fila$casos, 25)
})

test_that("`obtener_fila_mas_casos` maneja errores correctamente", {
  expect_error(
    obtener_meses_mas_casos(
      data_event = list(a = 1, b = 2),
      col_fechas = "ini_sin"
    ),
    "El parametro data_event debe ser un data.frame"
  )
})

test_that("`obtener_tip_ocurren_geo` funciona correctamente", {
  result_tip <- obtener_tip_ocurren_geo(cod_event = 210)
  expect_equal(result_tip, c(
    "cod_dpto_o", "departamento_ocurrencia",
    "cod_mun_o", "municipio_ocurrencia",
    "ocurrencia"
  ))
})

test_that("`obtener_tip_ocurren_geo` maneja errores correctamente", {
  expect_error(
    obtener_tip_ocurren_geo(),
    "Debe ingresar algun valor en cod_event o nombre_event"
  )
})

test_that("`obtener_info_depts` funciona correctamente", {
  result_dpto <- obtener_info_depts(dpto = "ANTIOQUIA")
  expect_s3_class(result_dpto, "data.frame")
  expect_equal(nrow(result_dpto), 125)

  result_mpio <- obtener_info_depts(dpto = "ANTIOQUIA", mpio = "MEDELLIN")
  expect_s3_class(result_mpio, "data.frame")
  expect_equal(nrow(result_mpio), 1)

  result_mpio <- obtener_info_depts(dpto = 05, mpio = "05001")
  expect_s3_class(result_mpio, "data.frame")
  expect_equal(nrow(result_mpio), 1)

  result_mpio <- obtener_info_depts(dpto = 05, mpio = 001)
  expect_s3_class(result_mpio, "data.frame")
  expect_equal(nrow(result_mpio), 1)

  result_mpio <- obtener_info_depts(
    dpto = "bogota dc",
    mpio = "bogota dc"
  )
  expect_s3_class(result_mpio, "data.frame")
  expect_equal(nrow(result_mpio), 1)
})

test_that("`obtener_info_depts` maneja errores correctamente", {
  expect_error(
    obtener_info_depts(),
    "El parametro dpto es obligatorio"
  )
  expect_error(
    obtener_info_depts(dpto = TRUE),
    "El parametro dpto debe ser una cadena de caracteres"
  )
})

test_that("`obtener_dptos` funciona correctamente", {
  result_dptos <- obtener_dptos()
  expect_equal(length(result_dptos), 34)
})

test_that("`obtener_nombre_dpto` funciona correctamente", {
  data_geo <- import_geo_cods()
  result_nomb <- obtener_nombre_dpto(data_geo,
    cod_dpto = "05"
  )
  expect_equal(result_nomb, "antioquia")
  result_nomb <- obtener_nombre_dpto(data_geo,
    cod_dpto = 05
  )
  expect_equal(result_nomb, "antioquia")
  result_nomb <- obtener_nombre_dpto(data_geo,
    cod_dpto = 8
  )
  expect_equal(result_nomb, "atlantico")
})

test_that("`obtener_nombre_dpto` maneja errores correctamente", {
  expect_error(
    obtener_nombre_dpto(list(a = 1, b = 2)),
    "El parametro data_geo debe ser un data.frame"
  )
  expect_error(
    obtener_nombre_dpto(data.frame(cod_dpto = 05, dpto = "antioquia")),
    "El parametro cod_dpto es obligatorio"
  )
})

test_that("`obtener_nombre_mpio` funciona correctamente", {
  data_geo <- import_geo_cods()
  result_nomb <- obtener_nombre_mpio(data_geo,
    cod_dpto = "05",
    cod_mpio = "001"
  )
  expect_equal(result_nomb, "medellin")
  result_nomb <- obtener_nombre_mpio(data_geo,
    cod_dpto = 05,
    cod_mpio = 001
  )
  expect_equal(result_nomb, "medellin")
  result_nomb <- obtener_nombre_mpio(data_geo,
    cod_dpto = 8,
    cod_mpio = 1
  )
  expect_equal(result_nomb, "barranquilla")
})

test_that("`obtener_nombre_mpio` maneja errores correctamente", {
  expect_error(
    obtener_nombre_mpio(list(a = 1, b = 2)),
    "El parametro data_geo debe ser un data.frame"
  )
  expect_error(
    obtener_nombre_mpio(data.frame(cod_dpto = 05, dpto = "antioquia")),
    "El parametro cod_dpto es obligatorio"
  )
  expect_error(
    obtener_nombre_mpio(data.frame(cod_dpto = 05, dpto = "antioquia"),
      cod_dpto = 05
    ),
    "El parametro cod_mpio es obligatorio"
  )
})

test_that("`obtener_cond_inciden_event` funciona correctamente", {
  condicion_incidencia <- obtener_cond_inciden_event(cod_eve = 210)

  expect_s3_class(condicion_incidencia, "data.frame")

  expect_true("cod_eve" %in% names(condicion_incidencia))
  expect_true("numerador" %in% names(condicion_incidencia))
  expect_true("condiciones_numerador" %in% names(condicion_incidencia))
  expect_true("denominador" %in% names(condicion_incidencia))
  expect_true("condiciones_denominador" %in% names(condicion_incidencia))
  expect_true("coeficiente" %in% names(condicion_incidencia))

  expect_equal(condicion_incidencia$numerador, "casos")
  expect_equal(condicion_incidencia$denominador, "riesgo")
  expect_equal(condicion_incidencia$coeficiente, 100000)
})

test_that("`obtener_cond_inciden_event` maneja errores correctamente", {
  expect_error(
    obtener_cond_inciden_event(),
    "El parametro cod_eve es obligatorio"
  )
})

test_that("`obtener_text_sex` funciona correctamente", {
  data_agrupada <- agrupar_sex(
    data_event = data_limpia,
    porcentaje = TRUE
  )
  text_sex <- obtener_text_sex(data_agrupada, year = 2020, figura = 3)

  expect_equal(
    text_sex$text,
    paste0(
      "En el total de casos para 2020 se ",
      "observa una predominancia del sexo ",
      "masculino con 53.19% respecto al sexo ",
      "femenino con 46.81% (Ver Figura 3)."
    )
  )
})

test_that("`obtener_text_sex` maneja errores correctamente", {
  expect_error(
    obtener_cond_inciden_event(),
    "El parametro cod_eve es obligatorio"
  )
})
