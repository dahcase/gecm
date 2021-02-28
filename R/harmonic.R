#' Compute harmonic regression
#'
#' @param r data cube
#' @param harmonic_value numeric. Number of harmonics to consider.
#' @param window numeric. Size of the window of consideration.
#'
#' Notes to self:
#' Window option likely assumes equally spaced layers. It doesn't take into account
#' any dates inherent in the layers.
#'
#'
harmonic <- function(r, harmonic_value, window, ...){
  UseMethod('harmonic', r)
}

harmonic.RasterStack <- function(r, harmonic_value = 2, window = NULL, ...){

}

harmonic.RasterBrick <- harmonic.RasterStack
