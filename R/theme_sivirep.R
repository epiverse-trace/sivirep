#' @title Tema sivirep
#' @description Tema \pkg{ggplot2} personalizado para los reportes
#' de \pkg{sivirep}.
#' @return Un objecto tema de \pkg{ggplot2}.
#' @keywords internal
tema_sivirep <- function() {
  sysfonts::font_add_google("Montserrat", "Montserrat")
  showtext::showtext_auto()
  tema <- ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "right",
      legend.direction = "vertical",
      plot.caption = ggplot2::element_text(size = 8),
      text = ggplot2::element_text(
        family = "Montserrat",
        size = 13
      ),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.title = ggplot2::element_text(face = "bold")
    )
  return(tema)
}

#' @title Obtener la estetica de una escala para un grafico de \pkg{sivirep}
#' @description Función que genera la estética de una escala para un gráfico
#' de \pkg{sivirep}.
#' @param escala Un `numeric` (numerico) que indica la cantidad de valores que
#' contiene la escala.
#' @param nombre Un `character` (cadena de caracteres) que contiene el nombre
#' de la escala.
#' @param etiquetas Un `character` (cadena de caracteres) que contiene las
#' etiquetas de la escala.
#' @returns Un objeto `scale_fill_manual` de \pkg{ggplot2}.
#' @keywords internal
obtener_estetica_escala <- function(escala = 0, nombre,
                                    etiquetas = NULL) {
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
      relleno_escala$labels <- etiquetas
    }
  }
  return(relleno_escala)
}
