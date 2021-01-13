#' Retrieve a categorical variable's rare levels.
#'
#' @param df A data frame.
#' @param varname_cat A character name of categorical variable.
#' @param thresh_frac_obs Numeric fraction of observations below which, category level considered 'rare'.
#'
#' @return Character indicator variable names.
#' @export

get_categories_rare <- function(df, varname_cat, thresh_frac_obs) {

  summ <- doPrepExplore::tabulate_frequency_overall(df, varnames_group = varname_cat)
  levels_rare <- summ %>%
    dplyr::filter(frac < thresh_frac_obs) %>%
    pull(!!rlang::sym(varname_cat))

  levels_rare

}
