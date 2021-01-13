#' Imputation using by-group mean.
#'
#' @param x Data frame with missing values, and those values for imputation calculations.
#' @param varnames_impute Vector of variable name(s) with missing values to be imputed.
#' @param varnames_grp Vector of variable name(s) used for grouping.
#' @param pre_df_means Existing data frame of by-group means, if available.
#' @return A list of associated imputation objects:
#' \itemize{
#'    \item (If pre_df_means not specified) df_means, data frame of by-group means.
#'    \item M, data frame of T/F, indicating: this cell was missing originally.
#'    \item df_impute, imputations overwritten for missing values of original input 'df'.
#' }
#'

impute_group_mean <- function(x, varnames_impute, varnames_grp, pre_df_means = NULL) {

  if (is.null(pre_df_means)) {

  varnames_grp.syms <- rlang::syms(varnames_grp)

  df_means <- x %>%
    group_by(!!!varnames_grp.syms) %>%
    dplyr::summarize(
      across(varnames_impute,
             .fns = list("mean" = ~ mean(.x, na.rm = T), "n" = ~ sum(!is.na(.x))),
             .names = "{.col}_{.fn}"))


  } else df_means <- pre_df_means


  #populate missing values ----

  x <- left_join(x, df_means)
  M <- is.na(df[, varnames_impute])
  for (varname_i in varnames_impute) {

    varname_mean_i <- paste(varname_i, "_mean", sep = "")
    x[M[, varname_i], varname_i] <- x[M[, varname_i], varname_mean_i]

  }
  x <- x %>%
    dplyr::select(-ends_with(c("_mean", "_n")))

  # ----


  if (is.null(pre_df_means)) out <- list("df_means" = df_means, "M" = M, "df_impute" = x)
  else out <- list("M" = M, "df_impute" = x)

  return(out)

}
