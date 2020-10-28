#' Consolidate sparsely-observed categorical levels, into 'other'.
#'
#' @param df Data frame with categorical variable whose sparsely-observed levels should be consolidated.
#' @param varname_cat Categorical variable name.
#' @param threshold Minimum share of observations with categorical variable value _v_, if _v_ to be kept.
#' @param value_consol Categorical variable _level_ into which sparsely-observed values will be consolidated.
#' @param construct_indic_other Does construct an indicator variable strictly to convey, 'is_other'.
#'
#' @return Original data frame, with new consolidated categorical variable ('_consol' suffix).


consolidate_sparse_other_cat <- function(df, varname_cat, threshold, value_consol = "OTHER", construct_indic_other = FALSE) {

  varname_cat.sym <- rlang::sym(varname_cat)

  summ <- df %>%
    group_by(!!varname_cat.sym) %>%
    summarize(n = n()) %>%
    arrange(desc(n))

  summ[["denom"]] <- sum(summ[["n"]])
  summ[["frac"]] <- summ[["n"]] / summ[["denom"]]
  summ[["frac_cumul"]] <- cumsum(summ[["frac"]])



  varname_cat_consol <- paste(varname_cat, "_consol", sep = "")
  varname_cat_consol.sym <- rlang::sym(varname_cat_consol)

  summ <- summ %>%
    mutate(!!varname_cat_consol.sym := ifelse(frac >= threshold, !!varname_cat.sym, value_consol))

  if (!construct_indic_other) {

    df <- left_join(df, summ %>% dplyr::select(!!varname_cat.sym, !!varname_cat_consol.sym))

  } else if (construct_indic_other) {

    varname_indic_other <- paste(varname_cat, value_consol, sep = "")
    varname_indic_other.sym <- rlang::sym(varname_indic_other)

    summ <- summ %>%
      mutate(!!varname_indic_other.sym := ifelse(!!varname_cat_consol.sym == value_consol, 1, 0))

    df <- left_join(df,
                    summ %>%
                      dplyr::select(!!varname_cat.sym, !!varname_indic_other.sym))

  }

  return(df)

}
