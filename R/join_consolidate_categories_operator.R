#' Join a categorical variable's operator for 'other' value consolidation, with no-joins also declared 'other'.
#'
#' Categorical variable consolidation operator is a data frame, mapping some category levels to 'other'.
#' Read operator from disk, as portable summary of 'train' reference.
#' Given a new data frame with initial categorical variable,
#' join the operator to construct consolidated categorical variable.
#'
#' @param filename_operator Filename of saved categorical variable consolidation operator.
#' @param df A data frame, with categorical variable to be consolidated.
#' @param varname_cat Character name of a categorical variable.
#' @param varname_suffix_consol Character suffix for categorical variable's consolidated version (contained in operator).
#' @param value_consol Value which constitutes the newly consolidated 'other' category.
#' @param varname_suffix_indic_other Character suffix for categorical variable's binary 'is other' version (contained in operator).
#'
#' @return Modified version of input data frame.
#' @export
#'

join_consolidate_categories_operator <-
  function(filename_operator, df,
           varname_cat, varname_suffix_consol = "consol", value_consol, varname_suffix_indic_other = "indic_other") {

  var_consolidations <- readRDS(filename_operator)

  df <- left_join(df, var_consolidations, by = varname_cat)

  #ASSUME: encode no-join categorical levels with 'other'.
  var_chk_na <- paste(varname_cat, "_", varname_suffix_consol, sep = "")
  if (var_chk_na %in% colnames(var_consolidations)) {
    M <- is.na(df[[var_chk_na]])
    df[[var_chk_na]][M] <- value_consol
  }

  var_chk_na <- paste(varname_cat, "_", varname_suffix_indic_other, sep = "")
  if (var_chk_na %in% colnames(var_consolidations)) {
    M <- is.na(df[[var_chk_na]])
    df[[var_chk_na]][M] <- 1
  }

  df

}
