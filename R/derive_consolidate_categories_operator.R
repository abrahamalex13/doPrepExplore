#' Derive a categorical variable's operator for 'other' value consolidation.
#'
#' Execute any combination of 'consolidate_categories_other' executions (use NULL threshold to not run a particular type).
#' After determining which category levels consolidate into 'other', save those mappings as an operator.
#'
#' @param df A data frame -- effectively, a 'train' reference.
#' @param varname_cat Character name of categorical variable.
#' @param thresh_nobs_consol For a modified version of original categorical variable --
#'     minimum count of observations with categorical level _v_, below which, _v_ enters 'other'.
#' @param thresh_nobs_indic_other For a binary indicator of 'other' --
#'     minimum count of observations with categorical level _v_, below which, _v_ enters 'other'.
#' @param value_consol For a modified version of original categorical variable -- v
#'     value which labels the newly consolidated 'other' category.
#' @param directory_operator Character directory for operator save.
#'
#' @return A list, containing:
#'     * A character filename of the constructed operator.
#'     * A data frame, showing whether a category level consolidates to 'other'.
#' @export
#'

derive_consolidate_categories_operator <-
  function(df, varname_cat,
           thresh_nobs_consol, thresh_nobs_indic_other, value_consol = "OTHER",
           directory_operator) {

    colnames_orig <- colnames(df)

    if (!is.null(thresh_nobs_indic_other)) {

      #an indicator of 'other' captures _main effect_
      #implied assumption: one broad 'OTHER' group may sufficiently describe various small-sample groups
      df <-
        doPrepExplore::consolidate_categories_other(df, varname_cat, threshold = thresh_nobs_indic_other / nrow(df),
                                                    value_consol = value_consol, construct_indic_other = TRUE)

    }

    if (!is.null(thresh_nobs_consol)) {

      #a consolidated var allows for some deviation from 'OTHER' main effect, _given sufficient sample size._
      #implied assumption: given a group with below-critical sample size, do not attempt to estimate dynamics.
      df <-
        doPrepExplore::consolidate_categories_other(df, varname_cat, threshold = thresh_nobs_consol / nrow(df),
                                                    value_consol = value_consol, construct_indic_other = FALSE)

    }


    #save consolidation operator under standardizing naming
    colnames_aug <- setdiff(colnames(df), colnames_orig)
    colnames_operator <- c(varname_cat, colnames_aug)
    operator <- df[, colnames_operator, drop = FALSE] %>% distinct()

    filename_operator <- mk_filename_op_consol(directory_operator = directory_operator, varname_cat = varname_cat)
    saveRDS(operator, file = filename_operator)

    list("path" = filename_operator,
         "op" = operator)

  }
