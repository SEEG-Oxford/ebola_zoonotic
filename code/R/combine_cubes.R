# calculate the mean and range from 2012 slices of Dan's cubes for LST and EVI

# clear workspace
rm(list = ls())

# load seegSDM
library(seegSDM)

# load a template raster
template <- raster('~/Z/zhi/friction_1km.tif')
# plot(template, maxpixels = 10000)

# set the input path ...
# inpath <- '~/Z/zhi/LST/day/'
# inpath <- '~/Z/zhi/LST/night/'
inpath <- '~/Z/zhi/EVI/'

# ...and the output path
# outpath <- '~/Z/zhi/LST_day_'
# outpath <- '~/Z/zhi/LST_night_'
outpath <- '~/Z/zhi/EVI_'

# import all of the relevant rasters
tmp <- importRasters(path = inpath,
                     as = stack,
                     ext = '.tif')

# crop the stack to Africa
tmp <- crop(tmp,
            template)

# set -9999 to NA
NAvalue(tmp) <- -9999

# plot to check that worked
plot(tmp,
     maxpixels = 10000)

# calculate the mean ...
tmp_mean <- calc(tmp,
                 fun = mean)

# ...and the range
tmp_range <- calc(tmp,
                  fun = range)

tmp_range <- tmp_range[[2]] - tmp_range[[1]]

# output these two files
writeRaster(tmp_mean,
            file = paste0(outpath,
                          'mean'),
            format = 'GTiff')

writeRaster(tmp_range,
            file = paste0(outpath,
                          'range'),
            format = 'GTiff')
