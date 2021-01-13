#' Interact a factor-type variable with another variable
#'
#' A factor-type variable interacted with another yields,
#' one new variable per factor level. New variable naming
#' follows a consistent `factor level`:`variable` naming convention.
#'
#' @param df A data frame.
#' @param pair_varnames Character vector of variable names to be interacted.
#'
#' @return A data frame, containing the newly constructed interactions.
#' @export

construct_factor_interactions <- function(df, pair_varnames) {

  #enforce consistent interactions naming
  value_expr <- paste("~ ", pair_varnames[1], ":", pair_varnames[2], " - 1", sep = "")

  as.data.frame( model.matrix(as.formula(value_expr), data = df, drop.unused.levels = TRUE) )

}
