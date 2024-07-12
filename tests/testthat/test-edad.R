data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)

test_that("`edad` funciona correctamente", {
  
  data_agrupada <- agrupar_edad(data_event = data_limpia,
                               porcentaje = TRUE)
  
  expect_s3_class(data_agrupada, "data.frame")
  expect_true("edad" %in% names(data_agrupada))
  expect_true("casos" %in% names(data_agrupada))
  expect_true("porcentaje" %in% names(data_agrupada))
  
  expect_equal(data_agrupada[["casos"]],
               c(7, 15, 11, 4, 4, 1, 2, 1))
  
  plot <- plot_edad(data_agrupada)
  expect_s3_class(plot, "ggplot")
  
})

test_that("`edad` maneja errores correctamente", {
  
  expect_error(agrupar_edad(data_event = list(a = 1, b = 2)),
               "El parametro data_event debe ser un data.frame")
  
  expect_error(agrupar_edad(data_event = data_limpia,
                            col_edad = TRUE),
               "El parametro col_edad debe ser una cadena de caracteres")
  
  expect_error(agrupar_edad(data_event = data_limpia,
                            porcentaje = "boolean"),
               "El parametro porcentaje debe ser un booleano")
  
  expect_error(plot_edad(data_agrupada = list(a = 1, b = 2)),
               "El parametro data_agrupada debe ser un data.frame")
  
  expect_error(plot_edad(data_agrupada = data_limpia,
                         col_edad = 1),
               "El parametro col_edad debe ser una cadena de caracteres")
})

test_that("`edad_sex` funciona correctamente", {
  
  data_agrupada <- agrupar_edad_sex(data_event = data_limpia,
                                    porcentaje = TRUE)
  
  expect_s3_class(data_agrupada, "data.frame")
  expect_true("edad" %in% names(data_agrupada))
  expect_true("sexo" %in% names(data_agrupada))
  expect_true("casos" %in% names(data_agrupada))
  expect_true("porcentaje" %in% names(data_agrupada))
  
  expect_equal(data_agrupada[["casos"]],
               c(4, 3, 6, 9, 4, 7, 3, 1, 1, 3, 1, 1, 1, 1))
  
  plot <- plot_edad_sex(data_agrupada)
  expect_s3_class(plot, "ggplot")
  
})

test_that("`edad_sex` maneja errores correctamente", {
  
  expect_error(agrupar_edad_sex(data_event = list(a = 1, b = 2)),
               "El parametro data_event debe ser un data.frame")
  
  expect_error(agrupar_edad_sex(data_event = data_limpia,
                                col_edad = TRUE),
               "El parametro col_edad debe ser una cadena de caracteres")
  
  expect_error(agrupar_edad_sex(data_event = data_limpia,
                                col_sex = TRUE),
               "El parametro col_sex debe ser una cadena de caracteres")
  
  expect_error(agrupar_edad_sex(data_event = data_limpia,
                                porcentaje = "boolean"),
               "El parametro porcentaje debe ser un booleano")
  
  expect_error(plot_edad_sex(data_agrupada = list(a = 1, b = 2)),
               "El parametro data_agrupada debe ser un data.frame")
  
  expect_error(plot_edad_sex(data_agrupada = data_limpia,
                             col_edad = 1),
               "El parametro col_edad debe ser una cadena de caracteres")
  
  expect_error(plot_edad_sex(data_agrupada = data_limpia,
                             col_edad = "edad", col_sex = 1),
               "El parametro col_sex debe ser una cadena de caracteres")
})
