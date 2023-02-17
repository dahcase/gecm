r = "C:/Users/Me/Documents/Ouagadougou_MOD13A1_006_NDVI_2001_2016.tif"

r = terra::rast(list())
r = terra::crop(r, raster::extent(as(r[[1]], 'Raster'), 1,2,1,2))

# r <- rast(ncols=10, nrows=10)
# values(r) <- 1
# x <- rast(lapply(1:100, function(x) r * runif(1)))
# s <- app(x, fun=function(z) c(sum(z), mean(z)))
# s2 <-  app(x, fun=function(z) harmonic_regression(z))
#


# s
# # for a few generic functions like
# # "sum", "mean", and "max" you can also do
# sum(x)
#
# ## SpatRasterDataset
# sd <- sds(r-.5, r, r+.5)
# a <- app(sd, sum)
# a
# # same as
# max(x, x*2, x/3)
