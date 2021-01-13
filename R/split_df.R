#' Split a data frame and return either train or test partition.
#'
#' @param df A data frame.
#' @param fraction_train Fraction of the dataset declared 'train'.
#' @param run_type Character indicating partition returned, 'train' or 'test'.
#'
#' @return
#' @export

split_df <- function(df, fraction_train, run_type) {

  index_row_train <- caret::createDataPartition(1:nrow(df), p = fraction_train, list = FALSE)
  if (run_type == "train") df[index_row_train[, 1], ]
  else if (run_type == "test") df[-index_row_train[, 1], ]

}
