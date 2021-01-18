#' Quickly visualize one discrete variable's level frequencies.
#'
#' @param df A data frame with plotting data.
#' @param varname_x Discrete variable name (character string).
#' @param order_x Do order variable levels along x-axis by frequency.
#' @param filename_print Character name of a plot file for save.
#'
#' @return

ggplot_bar_simple <- function(df, varname_x, order_x = TRUE, filename_print = NULL) {

  #need external frequencies data to declare sort via factor var
  if (order_x) df <- declare_factor_order_freq(df, varname_x)

  p <- ggplot(df) +
    geom_bar(aes_string(varname_x))
  if (!is.null(filename_print)) doPrepExplore:::save_plot_fmt_pdf(p, filename_print)

  p

}
