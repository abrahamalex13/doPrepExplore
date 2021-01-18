#' Re-label univariate plot's noted variable name.
#'
#' Variable names not typically optimal for plot presentations.
#' Use reference dictionary for re-label to human-friendly language.
#' This function helps primarily when producing many univariate charts.
#'
#' @param p ggplot object.
#' @param varname_x0 Initial (character string) variable name.
#' @param varnames_dict Dictionary (lookup table) matching original variable name (column 1) to polished name (column 2).
#' @param title_prefix Character for consistent chart titling.
#' @param lab_y Character for presentable y-axis.
#' @param lab_x Character for presentable x-axis.
#'
#' @return ggplot object.


relabel_univar_plot <- function(p, varname_x0, varnames_dict = NULL,
                                title_prefix = NULL, lab_y = NULL, lab_x = "Value") {

    if (varname_x0 %in% varnames_dict[[1]]) {
      varname_pres <- varnames_dict[varnames_dict[[1]] == varname_x0, 2, drop = TRUE]
    } else varname_pres <- varname_x0

    title <- paste(title_prefix, varname_pres, sep = "")

    out <- p + labs(title = title, y = lab_y, x = lab_x)

    return(out)
}
