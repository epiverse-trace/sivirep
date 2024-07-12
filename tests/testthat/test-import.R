test_that("`import_geo_cods` funciona correctamente", {
  
  geo_codes <- import_geo_cods()
  
  expect_s3_class(geo_codes, "data.frame")
  expect_true("codigo_departamento" %in% names(geo_codes))
  expect_true("nombre_departamento" %in% names(geo_codes))
  expect_true("codigo_municipio" %in% names(geo_codes))
  expect_true("codigo_municipio" %in% names(geo_codes))
  expect_true("tipo_municipio_isla_area_no_municipalizada" %in% names(geo_codes))
  
})

test_that("`import_geo_cods` maneja errores correctamente", {
  
  expect_error(import_geo_cods(descargar = 1),
               "El parametro descargar debe ser un booleano")
})

test_that("`list_events` funciona correctamente", {
  
  events <- list_events()
  
  expect_s3_class(events, "data.frame")
  expect_true("enfermedad" %in% names(events))
  expect_true("aa" %in% names(events))
})