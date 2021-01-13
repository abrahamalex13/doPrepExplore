#' Tabulate discrete variable(s).
#'
#' With a plan to append variable tabulation result
#' with other variables' tabulation results,
#' force to character, avoiding clashing data types.
#'
#' @param df Data frame with variable to be tabulated.
#' @param var Character variable name(s) to be tabulated.
#' @param rename_generic In tabulation result, does output
#'     a variable name-generic df, with columns: varname,
#'     value, n.
#' @return Data frame - a tabulation report.
#'
#' @export


tabulate_var_discrete <- function(df, var) {

  df %>%
    mutate(across(all_of(var), as.character)) %>%
    group_by(!!!rlang::syms(var)) %>%
    summarize(n = n()) %>%
    arrange(desc(n))

}
