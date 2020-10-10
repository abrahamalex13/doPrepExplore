#' ggplot a kernel density, with by-group flexibility and optional print-to-pdf.
#'
#' @param df Data frame with plotting data.
#' @param varname_x Variable name whose distribution will be plotted.
#' @param varname_grp Variable name used for grouping.
#' @param path_filename_print Optional pdf print file
#'
#' @return ggplot plot object.

ggplot_density_x <- function(df, varname_x, varname_grp = NULL, path_filename_print = NULL) {

  title <- paste("Density: ", varname_x, sep = "")
  varname_x.sym <- rlang::sym(varname_x)


  if (is.null(varname_grp)) {

    p <- ggplot(df) +
      geom_density(aes(!!varname_x.sym)) +
      labs(title = title)

  } else if (!is.null(varname_grp)) {

    varname_grp.sym <- rlang::sym(varname_grp)

    p <- ggplot(df) +
      geom_density(aes(!!varname_x.sym, group = !!varname_grp.sym, color = !!varname_grp.sym)) +
      labs(title = title)

  }


  if (!is.null(path_filename_print)) {

    pdf(path_filename_print, width = 13, height = 8)
    print(p)
    dev.off()

  }

  return(p)

}
