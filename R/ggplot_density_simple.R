#' Quickly visualize one continuous variable's distribution of values.
#'
#' @param df Data frame with plot data.
#' @param varname_x Character variable name.
#' @param filename_print Character name of a plot file for save.
#'
#' @return A ggplot object.

ggplot_density_simple <- function(df, varname_x, filename_print) {

  p <- ggplot(df) + geom_density(aes(!!rlang::sym(varname_x)))
  if (!is.null(filename_print)) doPrepExplore:::save_plot_fmt_pdf(p, filename_print)
  p

}
