data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_limpia <- estandarizar_geo_cods(data_limpia)

test_that("`dpto` funciona correctamente", {

  data_agrupada <- agrupar_dpto(data_event = data_limpia)

  expect_s3_class(data_agrupada, "data.frame")
  expect_true("cod_eve" %in% names(data_agrupada))
  expect_true("nombre_evento" %in% names(data_agrupada))
  expect_true("ano" %in% names(data_agrupada))
  expect_true("departamento_ocurrencia" %in% names(data_agrupada))
  expect_true("cod_dpto_o" %in% names(data_agrupada))
  expect_true("casos" %in% names(data_agrupada))
  
  expect_equal(data_agrupada[["casos"]],
               c(1, 12, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1))
  
  plot <- plot_dptos(data_agrupada)
  expect_s3_class(plot, "ggplot")
  
})

test_that("`dpto` maneja errores correctamente", {
  
  expect_error(agrupar_dpto(data_event = list(a = 1, b = 2)),
               "El parametro data_event debe ser un data.frame")
  
  expect_error(agrupar_dpto(data_event = data_limpia, col_dpto = TRUE),
               "El parametro col_dpto debe ser una cadena de caracteres")
  
  expect_error(agrupar_dpto(data_event = data_limpia,
                            porcentaje = "boolean"),
               "El parametro porcentaje debe ser un booleano")
  
  expect_error(plot_dptos(data_agrupada = list(a = 1, b = 2)),
               "El parametro data_agrupada debe ser un data.frame")
  
  expect_error(plot_dptos(data_agrupada = data_limpia,
                          col_dptos = 1),
               "El parametro col_dptos debe ser una cadena de caracteres")
})
