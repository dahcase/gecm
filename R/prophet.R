#' Run prophet and extract predictions
#'
#' @param x numeric. Vector of values to run prophet on
#' @param dates date. Vector of dates same length as \code{x} representing the dates that x correspond to
#' @param na.thresh numeric. Minimum proportional of missingness acceptable for a pixel
#' @param outputs character. The prophet results to be captured/returned
#' @param ... other arguments passed to prophet
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
#' @return rast
#' @export
prophet_rast = function(r, dates, ncores = 1, na.thresh = .25, outputs = c('yhat'), ..., filename = "", overwrite = FALSE, wopt = list()){

  #compute over the pixels
  #function(x) pixel_fit(x, dates, na.thresh = na.thresh, outputs, ...)
  dates = dates
  if(filename != ''){
    res = app(r, pixel_fit, dates = dates, na.thresh = na.thresh, outputs = outputs, ...,
              cores = ncores,filename = filename, overwrite = overwrite, wopt = wopt)
  }else{
    res = app(r, pixel_fit, dates = dates, na.thresh = na.thresh, outputs = outputs, ...,
              cores = ncores)
  }

  outnames = lapply(outputs, function(z) paste0(z, '_', dates))
  outnames = unlist(outnames)
  names(res) <- outnames

  return(res)
}
#' Compute anomalies from the results of prophet
#' @param yhat rast. Data cube of yhat predictions from \code{prophet_rast}
#' @param base rast. Data cube of "raw" values initially passed into \code{prophet_rast} as \code{r}
#' @param thresh numeric. Anomly threshold-- Number of standard deviations an estimate must be >= away from the mean
#' @param groups character. One or more of 'none', 'space', 'time', 'combined' detailing which anamoly data cubes should be returned
#' @param filename_prefix file path. Optional. Passed to terra::app, terra::patches, and a possible call to terra::writeRaster. File name (including directory) prefix for output geotiffs
#' @param overwrite logical. Optional. Passed to terra::app, terra::patches, and a possible call to terra::writeRaster
#' @param wotps named list. Optional. Passed to terra::app, terra::patches, and a possible call to terra::writeRaster. Options passed to writeRaster
#' @return a list of rast(bricks)
#' @export
find_anomaly = function(yhat, base, thresh = 1.65, groups = c('anomaly', 'space', 'time', 'combined'), filename_prefix = NULL, overwrite = F, wopts = NULL){
  grps = match.arg(groups, c('anomaly', 'space', 'time', 'combined'), several.ok = TRUE)
  stopifnot(thresh>0)

  # compute residuals and the associated standard deviation
  resid = yhat - base
  sd_oo = output_opts(filename_prefix, 'sdresid', overwrite, wopts)
  app_call = append(list(x = resid, fun = sd, na.rm = T), sd_oo)
  sdresid = do.call(app, app_call)

  # find anomalies
  anomaly = resid>(thresh*sdresid) | resid<(-1 * thresh *sdresid)

  # if only the base anomalies were requested
  if(all(grps == 'none')) return(sdresid)

  if(any(c('time', 'combined') %in% grps)){
    leadfun = function(x){
      lag = data.table::shift(x, type = 'lead') + x >=2
      return(lag)
    }
    at_oo = output_opts(filename_prefix, 'anomaly_time', overwrite, wopts)
    a_time_call = append(list(x = anomaly, fun = leadfun), at_oo)
    a_time_call = a_time_call[!sapply(a_time_call, is.null)]
    a_time = do.call(app, a_time_call)
  }
  if(any(c('space', 'combined') %in% grps)){
    as_oo = output_opts(filename_prefix, 'anomaly_space', overwrite,wopts)
    a_space_call = append(list(x = anomaly, zeroAsNA = FALSE), as_oo)
    a_space_call = a_space_call[!sapply(a_space_call, is.null)]
    a_space = do.call(terra::patches, a_space_call)
    a_space = a_space >0

  }

  if('combined' %in% grps){
    a_combo = a_space & a_time
  }

  pos_objs = c('anomaly' = 'anomaly', 'time' = 'a_time','space' = 'a_space', 'combined' = 'a_combo')

  obj = pos_objs[grps]
  obj = mget(obj)

  obj

}
#' A function to format options for saving rasters while processing
#' @param fp filepath prefix, including folder
#' @param name output name
#' @param overwrite logical. Optional. Passed to terra::writeRaster (or functions using it)
#' @param wotps named list. Optional.  Options passed to writeRaster (or functions using it)
output_opts = function(fp, name = 'rast', overwrite, wopts){
  if(is.null(fp)){
    return(NULL)
  }else{
    return(list(filename = file.path(fp, paste0(name, '.tif')),
                overwrite = overwrite,
                wopts = wopts))
  }
}

