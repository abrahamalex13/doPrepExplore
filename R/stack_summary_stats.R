#' Compute summary statistics and stack in an output data frame, with a "summary stat" label variable.
#'
#' @param df A data frame.
#' @param varnames_group A character vector of by-groups.
#' @param varnames_summarize A character vector of numeric variables to be summarized.
#' @param funs_summary List of numeric-geared functions, named.
#' @param filename_operator A complete file path for 'train' reference.
#'
#' @return Data frame of summary statistics.
#' @export

stack_summary_stats <- function(df, varnames_group, varnames_summarize = "all", funs_summary, filename_operator) {

  if (varnames_summarize == "all") {

    varnames_summarize <- colnames(df)[ unlist(lapply(df, is.numeric)) ]
    varnames_summarize <- varnames_summarize[!(varnames_summarize %in% varnames_group)]
  }

  summ <-
    purrr::map(funs_summary, df = df, varnames_summarize = varnames_summarize, varnames_group = varnames_group,
               function(.x, df, varnames_summarize, varnames_group) {

                 df %>%
                   group_by(!!!syms(varnames_group)) %>%
                   summarise_at(varnames_summarize, .funs = .x)

               })

  lbl_summary_stat <-
    unlist(
      lapply(1:length(summ), function(x) rep(names(funs_summary)[x], times = nrow(summ[[x]])))
    )

  summ <- bind_rows(summ)
  summ[["summary_statistic"]] <- lbl_summary_stat
  summ <- summ %>% dplyr::select(summary_statistic, everything())

  if (!is.null(filename_operator)) saveRDS(summ, filename_operator)
  summ

}
