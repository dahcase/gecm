#' Run prophet and extract predictions
#'
#' @param x numeric. Vector of values to run prophet on
#' @param dates date. Vector of dates same length as \code{x} representing the dates that x correspond to
#' @param na.thresh numeric. Minimum proportional of missingness acceptable for a pixel
#' @param outputs character. The prophet results to be captured/returned
#' @param ... other arguments passed to prophet
#' @details
#'
#' @importFrom prophet prophet
#' @importFrom data.table melt
#' @export
#'
pixel_fit = function(x, dates, na.thresh = .25, outputs, ...){

  #fraction NA
  na = sum(!is.na(x))/length(x)
  if(na >= na.thresh){

    dat = data.frame(ds = dates, y = x)
    p <- suppressMessages(prophet::prophet(dat, ...))

    #make predictions
    preds = predict(p, dat)
    data.table::setDT(preds)
    preds = preds[, .SD, .SDcols = c('ds', outputs)]

    preds = data.table::melt(preds, id.vars = 'ds')

    return(preds[, value])

  }else{
    return(rep(NA, length(outputs) * length(dates)))
  }
}

#' Run prophet on a terra data cube
#'
#' @param r rast. Data cube
#' @param dates vector of dates. the date each layer in \code{r} represents
#' @param ncores integer. NUmber of processes to use in parallel computing
#' @param na.thresh numeric. Minimum proportional of missingness acceptable for a pixel
#' @param outputs character. The prophet results to be captured/returned
#' @param ... other arguments passed to prophet
#' @param filename file path. Path to the output file, if using
#' @param overwrite logical. Determines whether filename gets overwritten
#' @param wopt list. Passed to terra::app
prophet_rast = function(r, dates, ncores = 1, na.thresh = .25, outputs = c('yhat'), ..., filename = NULL, overwrite = FALSE, wopt = list()){

  #compute over the pixels
  #function(x) pixel_fit(x, dates, na.thresh = na.thresh, outputs, ...)
  dates = dates
  res = app(r, pixel_fit, dates = dates, na.thresh = na.thresh, outputs = outputs, ...,
            cores = ncores, filename = filename, overwrite = overwrite, wopt = wopt)

  outnames = lapply(outputs, function(z) paste0(z, '_', dates))
  outnames = unlist(outnames)
  names(res) <- outnames

  return(res)
}
