#' Construct interaction of character variables, by pasting together.
#'
#' @param df Data frame with character variables to be interacted.
#' @param varnames_interact Vector of character variable names.
#' @return Original data frame, with new interacted (pasted) variable.


construct_interact_char <- function(df, varnames_interact) {

  varname_new <- paste(varnames_interact, collapse = "_")
  varname_new.sym <- rlang::sym(varname_new)

  varnames_interact.syms <- rlang::syms(varnames_interact)

  df <- df %>%
    mutate(!!varname_new.sym := paste(!!!varnames_interact.syms, sep = "_"))

  return(df)

}
