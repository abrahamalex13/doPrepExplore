#' For an interaction series following from a binary indicator and second variable, extract component series.
#'
#' @param varname_interact Character variable name of the interaction series.
#' @param df A data frame, containing the interaction series and underlying on/off switch.
#'
#' @return A list, containing:
#'     * The interaction series
#'     * The on/off interaction indicator series
#'
#'

get_interact_components <- function(varname_interact, df) {

  varname_indic_interact <- str_extract(varname_interact, "[^:]+")
  indic_interact <- ifelse(df[[varname_indic_interact]] == 1, TRUE, FALSE)

  list("interaction" = df[[varname_interact]],
       "indic_interaction" = indic_interact)

}
