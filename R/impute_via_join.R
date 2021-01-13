#' Impute NA values via join with reference data.
#'
#' @param x A data frame.
#' @param imputation_reference A data frame.
#' @param by Character vector of by-group variable names.
#'
#' @return Modified form of data frame `x`, with NA values imputed where possible.
#' @export

impute_via_join <- function(x, imputation_reference, by) {

  #a variable receives imputation attempt if:
    # it contains NA
    # it also occurs in the imputation_reference data
    # it is _not_ a grouping variable.
  varnames_some_na <- doPrepExplore::get_varnames_some_na(x)
  varnames_try_impute <-
    intersect(varnames_some_na,
              setdiff(colnames(imputation_reference), by) )

  #rename imputation reference variables,
  #from original matches to `x` names,
  #to clearly distinguish post-join.
  which_varnames_try_impute <-
    which(colnames(imputation_reference) %in% varnames_try_impute)
  varnames_try_impute_ren <-
    paste(colnames(imputation_reference)[which_varnames_try_impute],
          "_ref", sep = "")
  colnames(imputation_reference)[which_varnames_try_impute] <- varnames_try_impute_ren

  #impute where necessary
  x <- left_join(x, imputation_reference[, c(by, varnames_try_impute_ren)], by = by)
  for (varname in varnames_try_impute) {

    varname_ref <- paste(varname, "_ref", sep = "")

    M <- is.na(x[[varname]])
    x[[varname]][M] <- x[[varname_ref]][M]

    #reference values would clutter output df
    x <- x %>% dplyr::select(- !!sym(varname_ref) )

  }

  x

}
