data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)

test_that("`agrupar_semanaepi` funciona correctamente", {
  
  data_agrupada <- agrupar_semanaepi(data_limpia)
  expect_s3_class(data_agrupada, "data.frame")
  expect_equal(colnames(data_agrupada), c("semana", "casos"))
  
  expect_equal(data_agrupada[["casos"]],
               c(1, 1, 32, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 rep(NA, 39)))
})

test_that("`agrupar_semanaepi` maneja errores correctamente", {
  
  expect_error(agrupar_semanaepi(),
               "El parametro data_event es obligatorio")
  
  expect_error(agrupar_semanaepi(data_event = list(a = 1, b = 2)),
               "El parametro data_event debe ser un data.frame")
  
  expect_error(agrupar_semanaepi(data_event = data.frame()),
               "El parametro data_event no debe estar vacio")
  
  expect_error(agrupar_semanaepi(data_event = data_limpia,
                                 col_semanaepi = TRUE),
               "El parametro col_semanaepi debe ser una cadena de caracteres")
})
