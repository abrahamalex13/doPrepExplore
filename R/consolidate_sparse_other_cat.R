#' Consolidate sparsely-observed categorical levels, into 'other'.
#'
#' @param df Data frame with categorical variable whose sparsely-observed levels should be consolidated.
#' @param varname_cat Categorical variable name.
#' @param threshold Minimum share of observations with categorical variable value _v_, if _v_ to be kept.
#' @param value_consol Categorical variable _level_ into which sparsely-observed values will be consolidated.
#'
#' @return Original data frame, with new consolidated categorical variable ('_consol' suffix).
#'


#rationale: trust fixed effects estimates when
#a particular dummy var value has been observed many times.
consolidate_sparse_other_cat <- function(df, varname_cat, threshold, value_consol = "OTHER") {

  varname_cat.sym <- rlang::sym(varname_cat)

  varname_cat_consol <- paste(varname_cat, "_consol", sep = "")
  varname_cat_consol.sym <- rlang::sym(varname_cat_consol)



  summ <- df %>%
    group_by(!!varname_cat.sym) %>%
    summarize(n = n()) %>%
    arrange(desc(n))

  summ[["denom"]] <- sum(summ[["n"]])
  summ[["frac"]] <- summ[["n"]] / summ[["denom"]]
  summ[["frac_cumul"]] <- cumsum(summ[["frac"]])

  summ <- summ %>%
    mutate(!!varname_cat_consol.sym := ifelse(frac >= threshold, !!varname_cat.sym, value_consol))

  df <- left_join(df, summ %>% dplyr::select(!!varname_cat.sym, !!varname_cat_consol.sym))

  return(df)

}
