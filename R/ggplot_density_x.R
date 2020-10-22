#' Quickly ggplot a kernel density estimator, with optional print-to-pdf.
#'
#' @param df Data frame with plotting data.
#' @param varname_x Variable name (as character string) whose distribution will be plotted.
#' @param ... Other named arguments -- character string input values -- passing to ggplot2's aesthetics.
#' @param path_filename_print Optional pdf print file
#'
#' @return ggplot plot object..

ggplot_density_x <- function(df, varname_x, ..., path_filename_print = NULL) {

  title <- paste("Density: ", varname_x, sep = "")

  p <- ggplot(df) +
    geom_density(aes_string(x = varname_x, ...)) +
    labs(title = title)

  if (!is.null(path_filename_print)) {

    pdf(path_filename_print, width = 13, height = 8)
    print(p)
    dev.off()

  }

  return(p)

}
