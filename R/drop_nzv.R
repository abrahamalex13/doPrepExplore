#' Drop features with near/exactly zero variance, aligned with 'train' reference.
#'
#' @param df A data frame.
#' @param filename_operator A complete file path for 'train' reference.
#' @param run_type "train" or "test", dictating whether reference data saves to disk.
#'
#' @return A data frame, whose columns are a subset of input columns.
#' @export
#'

drop_nzv <- function(df, filename_operator, run_type) {

  if (run_type == "train") {

    nzv <- caret::nearZeroVar(df, saveMetrics = TRUE)
    saveRDS(nzv, filename_operator)

  }

  nzv <- readRDS(filename_operator)

  varnames_drop <- rownames(nzv)[which(nzv[["zeroVar"]] | nzv[["nzv"]])]
  df <- df[, -which(colnames(df) %in% varnames_drop)]

  df
}
