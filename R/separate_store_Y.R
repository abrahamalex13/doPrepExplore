#' Separate Y from input dataset, save on disk.
#'
#' @param df A data frame.
#' @param varname_y Character Y variable name.
#' @param directory Character folder name where Y saves.
#' @param run_type Character "train" or "test" (for filename).
#'
#' @return Modified input data frame, lacking Y variable.
#' @export
#'

separate_store_Y <- function(df, varname_y, directory, run_type) {

  filename <- paste(directory, "Y_", run_type, ".rds", sep = "")
  saveRDS(df[[varname_y]], file = filename)

  df[, -which(colnames(df) == varname_y)]

}
