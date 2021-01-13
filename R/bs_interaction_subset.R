#' B-Spline Basis for the interaction series' subset that actually varies.
#'
#' Drop interaction series values which reflect no dynamics (switch turned off),
#' then compute B-Spline Basis for the result.
#'
#' @param indic_interaction Boolean vector indicating which interaction series values are varying.
#' @param x Numeric vector of full interaction series (a subset exhibits meaningful variation).
#' @param ... Arguments to `splines::bs`.
#'
#' @return A `splines::bs` object (operator).
#' @export

bs_interaction_subset <- function(indic_interaction, x, ...) {

  x_sub <- x[indic_interaction]
  splines::bs(x_sub, ...)

}
