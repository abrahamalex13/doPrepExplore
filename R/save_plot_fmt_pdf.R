#' Print pdf plot(s) formatted with often-helpful defaults.
#'
#' @param p Plot object.
#' @param filename_print Character name of plot file for save.
#'
#' @return NULL.

save_plot_fmt_pdf <- function(p, filename_print) {

  pdf(filename_print, width = 13, height = 8)
  if (any(class(p) == "ggplot")) print(p)
  else if (class(p) == "list") invisible(lapply(p, FUN = print))
  dev.off()

  NULL

}
