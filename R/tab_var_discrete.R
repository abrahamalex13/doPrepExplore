#' Tabulate a discrete variable.
#'
#' With a plan to append variable tabulation result
#' with other variables' tabulation results,
#' force to character, avoiding clashing data types.
#'
#' @param df Data frame with variable to be tabulated.
#' @param varname Variable name to be tabulated.
#' @param rename_generic In tabulation result, does output
#'     a variable name-generic df, with columns: varname,
#'     value, n.
#' @return Data frame - a tabulation report.


tab_var_discrete <- function(df, varname, rename_generic = FALSE) {

  varname.sym <- rlang::sym(varname)

  out <- df %>%
    mutate(!!varname.sym := as.character(!!varname.sym)) %>%
    group_by(!!varname.sym) %>%
    summarize(n = n()) %>%
    arrange(desc(n))

  if (rename_generic) {

    out <- out %>%
      rename(value = !!varname.sym) %>%
      mutate(varname = varname) %>%
      dplyr::select(varname, value, n)

  }

  return(out)

}
