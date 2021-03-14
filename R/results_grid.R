#' Create a new object to store resutls in.
#'  @param dc data cube from which the reference layer will be created
#'  @param n_timesteps number of layers in the resulting datacube. usually relates to the time window.
#'  @param ... unused.
#'  @importFrom data.table as.data.table ":=" melt .I
#'  @importFrom terra nlyr values size add

new_gecm_results = function(dc, n_timesteps = 1, ...){
  UseMethod(dc)
}

new_gecm_results.SpatRaster = function(dc, n_timesteps = 1){

  if(!inherits(ras, 'SpatRaster')) stop('ras argument must be a raster')

  if(terra::nlyr(ras)>1) ras = ras[[1]]
  stopifnot(is.numeric(n_timesteps))

  if(n_timesteps>1){
    for(ttt in seq_len(n_timesteps)){
      terra::add(ras) <- ras
    }

  }

  #Give each cell a unique ID
  terra::values(ras) <- seq_len(terra::size(ras))
  names(ras) = paste0('lyr',seq_len(terra::nlyr(ras)))

  #convert the raster into a list col data.table to hold results
  res = as.data.frame(ras, cells = T, na.rm = F)
  data.table::setDT(res)
  res[, row := .I]
  res = data.table::melt(res, id.vars = 'row')
  res = res[, .(value)]
  res[, model := list(list())]

  #return
  g = list(ras, res)
  class(g) <- 'gecm_results'

  return(g)

}
#
#
# `[.gecm_results` <- function(x, row, col){
#
#   #subset the raster
#
#   #pass the subsetting onto the results grid
#
#   #return results grid with the name of the index
#
# }
