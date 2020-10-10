#' Polish chart labels for presentation, using variable names lookup table.
#'
#' Helpful primarily for scaled production of univariate charts.
#'
#' @param p ggplot object.
#' @param varname_x0 Initial variable name (to be polished).
#' @param df_relabels Lookup table matching original variable name to polished name.
#' @param title_prefix String for consistent chart titling.
#' @param lab_y String for presentable y-axis.
#' @param lab_x String for presentable x-axis.
#'
#' @return ggplot object.


relabel_plot_present <- function(p, varname_x0, df_relabels = NULL,
                              title_prefix = NULL, lab_y = NULL, lab_x = "Value") {

    if (varname_x0 %in% df_relabels[[1]]) varname_pres <- df_relabels[df_relabels[[1]] == varname_x0, 2, drop = TRUE]
    else varname_pres <- varname_x0

    title <- paste(title_prefix, varname_pres, sep = "")

    out <- p + labs(title = title, y = lab_y, x = lab_x)

    return(out)
  }


