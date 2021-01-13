#' Get the names of data frame variables containing NA values.
#'
#' @param x A data frame.
#'
#' @return Character vector.
#' @export

get_varnames_some_na <- function(x) {

  M <- is.na(x)
  tab_col_M <- apply(M, MARGIN = 2, sum)
  which_some_na <- which(tab_col_M > 0)
  varnames_some_na <- colnames(M)[which_some_na]

  varnames_some_na

}
