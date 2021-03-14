library('terra')
library('gecm')
r = "C:/Users/Me/Documents/Ouagadougou_MOD13A1_006_NDVI_2001_2016.tif"
r = terra::rast(r)
r = terra::crop(r, raster::extent(as(r[[1]], 'Raster'), 1,2,1,2))

harm = harmonic(r, 2, 'all', window = 23)
