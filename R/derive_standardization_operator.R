#' Derive a data frame standardization operator.
#'
#' Given a 'train' reference with variables to be standardized (mean zero, unit-variance),
#' derive and save the transforming operator.
#'
#' @param df A data frame.
#' @param filename_operator Character filename for operator save.
#'
#' @return A list, containing:
#'     * The character filename of the operator.
#'     * The operator object.
#' @export
#'

derive_standardize_operator <- function(df, filename_operator) {

  operator <- caret::preProcess(df, method = c("center", "scale"))
  saveRDS(operator, file = filename_operator)

  list("path" = filename_operator,
       "op" = operator)

}
