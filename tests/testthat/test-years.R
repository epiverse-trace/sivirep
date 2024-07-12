data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)

test_that("`year` funciona correctamente", {
  
  data_agrupada <- agrupar_years(data_event = data_limpia)
  
  expect_s3_class(data_agrupada, "data.frame")
  expect_true("cod_eve" %in% names(data_agrupada))
  expect_true("nombre_evento" %in% names(data_agrupada))
  expect_true("ano" %in% names(data_agrupada))
  expect_true("casos" %in% names(data_agrupada))
  
  expect_equal(data_agrupada[["casos"]], 45)
  
  plot <- plot_years(data_agrupada)
  expect_s3_class(plot, "ggplot")
  
})

test_that("`years` funciona correctamente", {
  
  data_years <- import_data_event(nombre_event = "MORTALIDAD MATERNA",
                                  years = seq(2019, 2020))
  data_limpia <- limpiar_data_sivigila(data_years)
  data_agrupada <- agrupar_years(data_event = data_limpia)

  expect_s3_class(data_agrupada, "data.frame")
  expect_true("cod_eve" %in% names(data_agrupada))
  expect_true("nombre_evento" %in% names(data_agrupada))
  expect_true("ano" %in% names(data_agrupada))
  expect_true("casos" %in% names(data_agrupada))
  
  expect_equal(data_agrupada[["casos"]], c(550, 607))
  
  plot <- plot_years(data_agrupada)
  expect_s3_class(plot, "ggplot")
  
})

test_that("`year` maneja errores correctamente", {
  
  expect_error(agrupar_years(data_event = list(a = 1, b = 2)),
               "El parametro data_event debe ser un data.frame")
  
  expect_error(agrupar_years(data_event = data_limpia, col_year = TRUE),
               "El parametro col_year debe ser una cadena de caracteres")

  expect_error(plot_years(data_agrupada = list(a = 1, b = 2)),
               "El parametro data_agrupada debe ser un data.frame")
  
  expect_error(plot_years(data_agrupada = data_limpia,
                          col_year = 1),
               "El parametro col_year debe ser una cadena de caracteres")
})
