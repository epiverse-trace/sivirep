data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)

test_that("`per_etn` funciona correctamente", {
  
  data_agrupada <- agrupar_per_etn(data_event = data_limpia)
  
  expect_s3_class(data_agrupada, "data.frame")
  expect_true("cod_eve" %in% names(data_agrupada))
  expect_true("nombre_evento" %in% names(data_agrupada))
  expect_true("ano" %in% names(data_agrupada))
  expect_true("per_etn" %in% names(data_agrupada))
  expect_true("nombre_per_etn" %in% names(data_agrupada))
  expect_true("casos" %in% names(data_agrupada))
  expect_true("porcentaje" %in% names(data_agrupada))
  
  expect_equal(data_agrupada[["casos"]], c(1, 1, 43))
  
  plot <- plot_per_etn(data_agrupada)
  expect_s3_class(plot, "ggplot")
  
})

test_that("`per_etn` maneja errores correctamente", {
  
  expect_error(agrupar_per_etn(data_event = list(a = 1, b = 2)),
               "El parametro data_event debe ser un data.frame")
  
  expect_error(agrupar_per_etn(data_event = data_limpia, cols_etn = TRUE),
               "El parametro cols_etn debe ser una cadena de caracteres")
  
  expect_error(agrupar_per_etn(data_event = data_limpia,
                               porcentaje = "boolean"),
               "El parametro porcentaje debe ser un booleano")
  
  expect_error(plot_per_etn(data_agrupada = list(a = 1, b = 2)),
               "El parametro data_agrupada debe ser un data.frame")
  
  expect_error(plot_per_etn(data_agrupada = data_limpia,
                            col_etn = 1),
               "El parametro col_etn debe ser una cadena de caracteres")
})
