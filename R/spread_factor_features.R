#' Spread each factor-type variable to binary indicator columns.
#'
#' One factor level maps to one binary indicator.
#'
#' @param df A data frame.
#' @param omit_one_level One factor level is not spread (avoids perfect collinearity)
#'
#' @return A data frame.
#' @export
#'

spread_factor_features <- function(df, omit_one_level = FALSE) {

  if (!omit_one_level) {

    #ensure every factor level expands to a feature - perfect collinearity follows
    operator <- caret::dummyVars(~ ., data = df, sep = "")
    X <- as.data.frame( predict(operator, df) )

  } else X <- model.matrix(~ . - 1, df)

  return(X)

}
