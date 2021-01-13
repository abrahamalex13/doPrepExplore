#' Evaluate Spline Basis for an interaction series subset that actually varies
#'
#' Drop interaction series values which reflect no dynamics (switch turned off),
#' then evaluate B-Spline Basis for the result. Save the evaluation
#' in the original interaction series' context, among values which reflect no dynamics.
#'
#' @param object A `splines::bs` object.
#' @param indic_interaction Boolean vector indicating which interaction series values are varying.
#' @param newx The interaction `x` values, containing a subset at which evaluations are required.
#'
#' @return A data frame of spline basis evaluations, saved within original interaction series' on/off form.
#' @export
#'

predict_bs_interaction_subset <- function(object, indic_interaction, newx) {

  which_fill <- which(indic_interaction)
  newx_sub <- newx[indic_interaction]

  trfm <- splines:::predict.bs(object, newx = newx_sub)

  out <- matrix(0, ncol = ncol(trfm), nrow = length(newx))
  out[which_fill, ] <- trfm
  as.data.frame(out)

}
