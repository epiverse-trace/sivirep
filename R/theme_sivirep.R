#' @title Tema sivirep
#' @description Tema \pkg{ggplot2} personalizado para los reportes
#' de \pkg{sivirep}.
#' @return Un objeto tema de \pkg{ggplot2}.
#' @keywords internal
tema_sivirep <- function() {
  sysfonts::font_add_google("Montserrat", "Montserrat")
  showtext::showtext_auto()
  tema <- ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "right",
      legend.direction = "vertical",
      plot.caption = ggplot2::element_text(size = 12),
      text = ggplot2::element_text(
        family = "Montserrat",
        size = 14.5
      ),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.title = ggplot2::element_text(face = "bold"),
      plot.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(face = "bold",
                                            hjust = 0.5)
    )
  return(tema)
}

#' @title Obtener la estética de una escala para un gráfico de \pkg{sivirep}
#' @description Función que genera la estética de una escala para un gráfico
#' de \pkg{sivirep}.
#' @param escala Un `numeric` (numérico) que indica la cantidad de valores que
#' contiene la escala.
#' @param nombre Un `character` (cadena de caracteres) que contiene el nombre
#' de la escala.
#' @param etiquetas Un `character` (cadena de caracteres) que contiene las
#' etiquetas de la escala.
#' @return Un objeto `scale_fill_manual` de \pkg{ggplot2}.
#' @keywords internal
obtener_estetica_escala <- function(escala = 0, nombre,
                                    etiquetas = NULL,
                                    ajustar_texto = FALSE) {
  colores <- c(
    "#2274BB", "#5ab4ac", "#d8b365", "#AC6DAD", "#D49392",
    "#19AFE5", "#87C762", "#9DB2D0"
  )
  colores <- colores[1:escala]
  if (escala > 0) {
    relleno_escala <- ggplot2::scale_fill_manual(
      values = colores,
      name = nombre
    )
    if (!is.null(etiquetas)) {
      if (ajustar_texto) {
        relleno_escala$labels <-
          stringr::str_wrap(etiquetas, 5)
      } else {
        relleno_escala$labels <- etiquetas
      }
    }
  }
  return(relleno_escala)
}
