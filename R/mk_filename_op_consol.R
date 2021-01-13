#' Make consolidation operator filename, according to centralized naming convention.

mk_filename_op_consol <- function(directory_operator, varname_cat) {

  paste(directory_operator, varname_cat, "_consolidations.rds", sep = "")

}
