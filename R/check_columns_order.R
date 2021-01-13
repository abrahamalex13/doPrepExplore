#' Record/apply a data frame's columns order.
#'
#' Record or apply an operator to enforce specific column names and order,
#' according to a 'train' reference.
#'
#' @param df A data frame.
#' @param filename_operator Character filename for operator save.
#' @param run_type "train" or "test", dictating whether reference data saves to disk.
#'
#' @return A data frame, with ordered columns.
#' @export

check_columns_order <- function(df, filename_operator, run_type) {

  if (run_type == "train") saveRDS(colnames(df), filename_operator)
  else {

    operator <- readRDS(filename_operator)
    df <- df[, operator]

  }

  df

}
