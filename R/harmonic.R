#' Compute harmonic regression
#'
#' @param r data cube
#' @param harmonic_value numeric. Number of harmonics to consider.
#' @param variable character. Name of the variable/result to return. See details for more information.
#' @param window numeric. Size of the window of consideration. Chunks the input data cube into cubes with `window` layers/timesteps
#' @param filename_prefix file path. Optional. Passed to terra::app, terra::patches, and a possible call to terra::writeRaster. File name (including directory) prefix for output geotiffs
#' @param overwrite logical. Optional. Passed to terra::app
#' @param wotps named list. Optional. Passed to terra::app
#'
#'
#' @details
#' The following values are possibly returned from harmonic:
#'    0. 'all': return all valid terms
#'    1. 'A0': Average; base mean change in the system
#'    2. 'A1': Period specific seasonality
#'    3. 'A2': Within period seasonality
#'    4. 'P1': Timing of A1 peak (e.g. peak of season)
#'    5. 'P2': Timing of A2 peak (e.g. change in dynamics)
#'
#'
#' @importFrom terra nlyr
#' @export
harmonic <- function(r, harmonic_value = 2, variable = 'all', window = NULL, filename_prefix = NULL, overwrite = F, wopts = NULL){

  stopifnot(inherits(r, 'SpatRaster'))

  #validate harmonic value
  stopifnot(is.numeric(harmonic_value) && length(harmonic_value) >0 &&
              harmonic_value >0 && harmonic_value == floor(harmonic_value))
  #variable
  var_choices = c('all', 'A', 'B', 'A0', 'phase', 'omega')
  stopifnot(all(variable %in% var_choices))
  if(any(variable %in% 'all')){
    variable = setdiff(var_choices, 'all')
  }

  #split the input raster into chunks
  stopifnot(is.null(window) || (is.numeric(window) & length(window) == 1))
  if(is.null(window)) window = terra::nlyr(r)
  chnks = split(seq_len(terra::nlyr(r)), ceiling(seq_len((terra::nlyr(r))) / window))

  #for each group, compute things
  res = lapply(chnks, function(x){

    ras = terra::subset(r, x)

    #for each cell, compute the various doodads
    if(!is.null(filename_prefix)){
      ras = terra::app(ras, function(y) harmonic_regression(y, harmonic_value = harmonic_value, variable = variable),
                       filename = file.path(filename_prefix, paste0(min(x),'-',max(x),'.tif')), overwrite = overwrite,
                       wopts = wopts)

    }else{
      ras = terra::app(ras, function(y) harmonic_regression(y, harmonic_value = harmonic_value, variable = variable))

    }

    ras
  })

  #rename the results

  for(x in seq_along(res)) {
    p = seq_len(harmonic_value)
    nnn <- c('A0',
             paste0('A', p),
             paste0('a', p),
             paste0('b', p),
             'p_A0',
             paste0('p_a', p),
             paste0('p_b', p),
             paste0('phase', p),
             paste0('omega', p))

    nnn = paste0('w',x,'_',nnn)

    names(res[[x]]) = nnn

  }
  nnn = lapply(res, names)
  nnn = unlist(nnn)

  res = terra::rast(res)
  names(res) <- nnn

  res

}

#' @rdname harmonic
harmonic_regression = function(y, harmonic_value = 2, variable = 'all'){

  if(!all(is.na(y)) && length(y)>1){

    p = seq_len(harmonic_value)

    #construct the model frame
    t = 1:length(y)
    omega_val = 2*pi*p/length(y)
    cos_val <- lapply(omega_val,function(omega){cos(omega*t)})
    names(cos_val) = paste0('cos',p )
    sin_val <- lapply(omega_val,function(omega){sin(omega*t)})
    names(sin_val) <- paste0('sin', p)


    mf = data.frame(y, cos_val, sin_val)

    #fit the model
    f = paste0('y~', paste(names(mf)[-1], collapse = ' + '))
    mod = lm(formula = f, data = mf)

    #clean the coefficients
    betas = coef(mod)
    a = betas[paste0('sin', p)]
    b = betas[paste0('cos', p)]

    #compute and organize return values
    A = sqrt(a ^2 + b ^ 2)
    phase = atan(-b/a)
    omega = omega_val
    A0 = betas[1]
    pvals = summary(mod)$coefficients[,'Pr(>|t|)']

    vals = c(A0, A, a, b, pvals, phase, omega_val)

  }else{
    vals = rep(NA_real_, 2 + 7 * length(p))
  }

  names(vals) = NULL

  return(vals)

}
