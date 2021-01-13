#' Consolidate categorical variable's rarely observed levels into 'other'.
#'
#' Two options, controlled by `construct_indic_other`:
#'    * (FALSE) Construct modified version of original categorical variable.
#'    Overwrite some level values with 'other', according to minimum frequency threshold.
#'    Modified variable result takes '_consol' suffix.
#'    * (TRUE) Construct binary indicator variable strictly to convey, 'is other',
#'    according to minimum frequency threshold.
#'
#' @param df A data frame, with categorical variable whose rarely observed levels should be consolidated.
#' @param varname_cat Character name of categorical variable.
#' @param threshold Minimum share of observations with categorical level _v_, below which, _v_ enters 'other'.
#' @param value_consol Value which constitutes the newly consolidated 'other' category.
#' @param construct_indic_other Does construct binary indicator variable strictly to convey, 'is_other'.
#'
#' @export
#'
#' @return Modified version of input data frame.


consolidate_categories_other <- function(df, varname_cat, threshold, value_consol = "OTHER", construct_indic_other = FALSE) {

  summ <- doPrepExplore::tabulate_frequency_overall(df = df, varnames_group = varname_cat)

  varname_cat.sym <- rlang::sym(varname_cat)
  varname_cat_consol <- paste(varname_cat, "_consol", sep = "")
  varname_cat_consol.sym <- rlang::sym(varname_cat_consol)

  summ <- summ %>%
    mutate(!!varname_cat_consol.sym := ifelse(frac >= threshold, !!varname_cat.sym, value_consol))

  if (!construct_indic_other) {

    df <- left_join(df, summ %>% dplyr::select(!!varname_cat.sym, !!varname_cat_consol.sym))

  } else if (construct_indic_other) {

    varname_indic_other <- paste(varname_cat, "indic_other", sep = "_")
    varname_indic_other.sym <- rlang::sym(varname_indic_other)

    summ <- summ %>%
      mutate(!!varname_indic_other.sym := ifelse(!!varname_cat_consol.sym == value_consol, 1, 0))

    df <- left_join(df,
                    summ %>%
                      dplyr::select(!!varname_cat.sym, !!varname_indic_other.sym))

  }

  return(df)

}
