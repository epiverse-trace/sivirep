data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)

test_that("`incidencia_geo` funciona correctamente", {
  
  data_agrupada <- agrupar_mpio(data_event = data_limpia,
                                dpto = "Antioquia")
  incidencia_mpios <- calcular_incidencia_geo(data_agrupada =
                                                data_agrupada,
                                              year = 2020)
  
  expect_s3_class(incidencia_mpios, "data.frame")
  expect_true("cod_eve" %in% names(data_agrupada))
  expect_true("nombre_evento" %in% names(data_agrupada))
  expect_true("ano" %in% names(data_agrupada))
  expect_true("departamento_ocurrencia" %in% names(incidencia_mpios))
  expect_true("cod_dpto_o" %in% names(incidencia_mpios))
  expect_true("municipio_ocurrencia" %in% names(incidencia_mpios))
  expect_true("cod_mun_o" %in% names(incidencia_mpios))
  expect_true("casos" %in% names(incidencia_mpios))
  expect_true("incidencia" %in% names(incidencia_mpios))
  
  expect_equal(data_agrupada[["casos"]], incidencia_mpios[["casos"]])
  expect_equal(incidencia_mpios[["incidencia"]],
               c(0.16, 0.78, 1.48, 2.27, 5.27, 4.77, 2.54, 9.33, 0.77))
  
  tabla <- plot_tabla_incidencia_geo(data_agrupada = incidencia_mpios,
                                     col_geo = "municipio_ocurrencia")
  
  expect_s3_class(tabla, "kableExtra")
  
})

test_that("`incidencia_geo` maneja errores correctamente", {
  
  expect_error(calcular_incidencia_geo(data_agrupada = list(a = 1, b = 2)),
               "El parametro data_agrupada debe ser un data.frame")
  
  expect_error(plot_tabla_incidencia_geo(list(a = 1, b = 2)),
               "El parametro data_agrupada debe ser un data.frame")
})

test_that("`incidencia_sex` funciona correctamente", {
  
  data_agrupada <- agrupar_sex(data_event = data_limpia)
  incidencia_sex <-
        calcular_incidencia_sex(data_agrupada = data_agrupada,
                                dpto = "Antioquia")
  
  expect_s3_class(incidencia_sex, "data.frame")
  expect_true("cod_eve" %in% names(data_agrupada))
  expect_true("nombre_evento" %in% names(incidencia_sex))
  expect_true("sexo" %in% names(incidencia_sex))
  expect_true("casos" %in% names(incidencia_sex))
  expect_true("incidencia" %in% names(incidencia_sex))
  
  expect_equal(data_agrupada[["casos"]], incidencia_sex[["casos"]])
  expect_equal(incidencia_sex[["incidencia"]],
               c(0.61, 0.75))
  
  tabla <- plot_tabla_incidencia_sex(data_agrupada = incidencia_sex)
  expect_s3_class(tabla, "kableExtra")
  
})

test_that("`incidencia_sex` maneja errores correctamente", {
  
  expect_error(calcular_incidencia_sex(data_agrupada = list(a = 1, b = 2)),
               "El parametro data_agrupada debe ser un data.frame")
  
  expect_error(plot_tabla_incidencia_sex(list(a = 1, b = 2)),
               "El parametro data_agrupada debe ser un data.frame")
})
