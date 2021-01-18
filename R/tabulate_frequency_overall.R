#' Tabulate frequencies for grouping variable(s).
#'
#' @param df A data frame.
#' @param varnames_group Character name(s) of variables for frequency tabulation.
#' @param compute_shares TRUE/FALSE, do compute group-wise frequencies as fraction of overall.
#'
#' @return A data frame with summary frequencies.
#' @export

tabulate_frequency_overall <- function(df, varnames_group, compute_shares = TRUE) {

  summ <- df %>%
    group_by(!!!rlang::syms(varnames_group)) %>%
    summarize(n = n()) %>%
    arrange(desc(n))

  summ[["denom"]] <- sum(summ[["n"]])
  summ[["frac"]] <- summ[["n"]] / summ[["denom"]]
  summ[["frac_cumul"]] <- cumsum(summ[["frac"]])

  summ

}
