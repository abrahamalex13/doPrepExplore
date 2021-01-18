#' Declare factor-type variable using underlying frequencies.
#'
#' @param df A data frame with variable to become factor-type.
#' @param varname_x Character variable name.
#'
#' @return Modified version of input data frame.

declare_factor_order_freq <- function(df, varname_x) {

  #need external frequencies data to declare sort via factor var
  tab_x <- doPrepExplore::tabulate_frequency_overall(df, varname_x, compute_shares = FALSE)
  df[[varname_x]] <- factor(df[[varname_x]], levels = tab_x[[varname_x]])

  df

}
