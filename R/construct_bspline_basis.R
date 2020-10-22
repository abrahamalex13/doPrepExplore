#' Construct B-Spline basis functions.
#'
#' Adopt HRW's ZOSull spline knots defaults,
#' and apply to splines' B-spline basis generator.
#'
#' @param x Original predictor variable.
#' @param varnames_interact Vector of character variable names.
#' @return B-spline ('bs') object.


construct_bspline_basis <- function(x, knots, varnames_out_prefix) {

  if (missing(knots)) {

    #from HRW::ZOSull.
    numIntKnots <- min(length(unique(x)), 35)
    intKnots <-
      quantile(unique(x), seq(0, 1, length = (numIntKnots + 2))[-c(1, (numIntKnots + 2))])
    knots <- intKnots

  }

  splines_x <- splines::bs(x, knots = knots)

  if (!missing(varnames_out_prefix)) {

    colnames(splines_x) <- paste(varnames_out_prefix, "_bs", 1:ncol(splines_x), sep = "")

  }

  return(splines_x)

}
