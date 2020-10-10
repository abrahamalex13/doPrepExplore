#' Construct separate month-day-year variables, from single combined date variable.
#'
#' @param df Data frame with date variable(s) to be manipulated.
#' @param varnames_date Vector of original combined date variable names.
#'
#' @return Data frame with both original and new date variables.

construct_vars_mdy <- function(df, varnames_date) {

  df_out <- df %>%
    mutate(across(varnames_date,
                  .fns = list("year" = lubridate::year,
                              "month" = lubridate::month,
                              "day" = lubridate::day),
                  .names = "{.col}_{.fn}"))

  return(df_out)
}
