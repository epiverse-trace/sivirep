data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)

test_that("`area_geo` funciona correctamente", {
  
  data_agrupada <- agrupar_area_geo(data_event = data_limpia)
  
  expect_s3_class(data_agrupada, "data.frame")
  expect_true("cod_eve" %in% names(data_agrupada))
  expect_true("nombre_evento" %in% names(data_agrupada))
  expect_true("ano" %in% names(data_agrupada))
  expect_true("area" %in% names(data_agrupada))
  expect_true("casos" %in% names(data_agrupada))
  
  expect_equal(data_agrupada[["casos"]], c(36, 5, 4))
  
  plot <- plot_area_geo(data_agrupada)
  expect_s3_class(plot, "ggplot")
  
})

test_that("`area_geo` maneja errores correctamente", {
  
  expect_error(agrupar_area_geo(data_event = list(a = 1, b = 2)),
               "El parametro data_event debe ser un data.frame")
  
  expect_error(agrupar_area_geo(data_event = data_limpia,
                                col_area = TRUE),
               "El parametro col_area debe ser una cadena de caracteres")
  
  expect_error(agrupar_area_geo(data_event = data_limpia,
                                porcentaje = "boolean"),
               "El parametro porcentaje debe ser un booleano")
  
  expect_error(plot_area_geo(data_agrupada = list(a = 1, b = 2)),
               "El parametro data_agrupada debe ser un data.frame")
  
  expect_error(plot_area_geo(data_agrupada = data_limpia,
                             col_area = 1),
               "El parametro col_area debe ser una cadena de caracteres")
})

test_that("`top_area_geo` funciona correctamente", {
  
  data_agrupada <- agrupar_top_area_geo(data_event = data_limpia,
                                        dpto = "Antioquia")
  
  expect_s3_class(data_agrupada, "data.frame")
  expect_true("cod_eve" %in% names(data_agrupada))
  expect_true("nombre_evento" %in% names(data_agrupada))
  expect_true("ano" %in% names(data_agrupada))
  expect_true("area" %in% names(data_agrupada))
  expect_true("departamento_ocurrencia" %in% names(data_agrupada))
  expect_true("cod_dpto_o" %in% names(data_agrupada))
  expect_true("municipio_ocurrencia" %in% names(data_agrupada))
  expect_true("cod_mun_o" %in% names(data_agrupada))
  expect_true("casos" %in% names(data_agrupada))
  
  expect_equal(data_agrupada[["casos"]], c(3, rep(1, 9)))
  
  plot <- plot_top_area_geo(data_agrupada)
  expect_s3_class(plot, "ggplot")
  
})

test_that("`top_area_geo` maneja errores correctamente", {
  
  expect_error(agrupar_top_area_geo(data_event = list(a = 1, b = 2)),
               "El parametro data_event debe ser un data.frame")
  
  expect_error(agrupar_top_area_geo(data_event = data_limpia,
                                    col_area = TRUE),
               "El parametro col_area debe ser una cadena de caracteres")
  
  expect_error(agrupar_top_area_geo(data_event = data_limpia,
                                    porcentaje = "boolean"),
               "El parametro porcentaje debe ser un booleano")
  
  expect_error(plot_top_area_geo(data_agrupada = list(a = 1, b = 2)),
               "El parametro data_agrupada debe ser un data.frame")
  
  expect_error(plot_top_area_geo(data_agrupada = data_limpia,
                                 col_area = 1),
               "El parametro col_area debe ser una cadena de caracteres")
})
