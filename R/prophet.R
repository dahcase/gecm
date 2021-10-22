#' Compute prophet based metrics of change
#'
#' @param r data cube
#' @param nalevel numeric. Minimum proportional of missingness acceptable for a pixel
#' @param threshold numeric. A two item vector of percentiles representing the bounds outside of which residuals are considered anomalous
#' @param ... other arguments passed to prophet
#'
#'
#' @details
#'
#' @importFrom terra nlyr
#' @export
#'

prophet_change = function(r, nalevel = .1, threshold = c(.05,.95)){}
