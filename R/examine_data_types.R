#' Examine data types, compare to 'train' reference.
#'
#' Collect data types in a data.frame.
#' If 'train' execution, save that summary.
#' If 'test' execution, confirm equality with 'train' reference.
#'
#' @param df A data frame.
#' @param filename_operator A complete file path for 'train' reference.
#' @param run_type "train" or "test", dictating whether reference data saves to disk.
#'
#' @return Vector: character entries reflect data types, names reflect variable names.
#' @export


examine_data_types <- function(df, filename_operator, run_type) {

  data_types <- caret:::get_types(df, coarse = TRUE)

  if (run_type == "train") {

    saveRDS(data_types, file = filename_operator)

  } else if (run_type == "test") {

    data_types_expected <- readRDS(filename_operator)
    if (any(data_types != data_types_expected)) stop("New data types do not match expected from train.")

  }

  data_types
}
