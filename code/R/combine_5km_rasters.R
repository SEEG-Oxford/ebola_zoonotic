# combine the 5km rasters of interest

# clear workspace
rm(list = ls())

# load packages
library(raster)

# list the covariates of interest
covars <- c('~/Z/zhi/ebola/5km_spatial_summary_rasters/EVI/mean/annual.synoptic.mean.tif',
            '~/Z/zhi/ebola/5km_spatial_summary_rasters/EVI/range/annual.synoptic.mean.tif',
            '~/Z/zhi/ebola/5km_spatial_summary_rasters/LST_day/mean/annual.synoptic.mean.tif',
            '~/Z/zhi/ebola/5km_spatial_summary_rasters/LST_day/range/annual.synoptic.mean.tif',
            '~/Z/zhi/ebola/5km_spatial_summary_rasters/LST_night/mean/annual.synoptic.mean.tif',
            '~/Z/zhi/ebola/5km_spatial_summary_rasters/LST_night/range/annual.synoptic.mean.tif',
            '~/Z/zhi/ebola/5km_spatial_summary_rasters/WorldClim_fourier_precip/prec57a0.tif',
            '~/Z/zhi/ebola/5km_spatial_summary_rasters/WorldClim_fourier_precip/prec57a1.tif',
            '~/Z/zhi/ebola/5km_spatial_summary_rasters/poverty/accessibility_50k_5km.mean.tif',
            '~/Z/zhi/ebola/5km_spatial_summary_rasters/poverty/population_density_5km.mean.tif',
            '~/Z/zhi/ebola/5km_spatial_summary_rasters/poverty/PET_1950-2000_5km.mean.tif',
            '~/Z/zhi/ebola/5km_spatial_summary_rasters/poverty/srtm_1km_5km.mean.tif',
            '~/Z/zhi/ebola/5km_spatial_summary_rasters/TCW/mean/annual.synoptic.mean.tif',
            '~/Z/zhi/ebola/5km_spatial_summary_rasters/TCB/mean/annual.synoptic.mean.tif',
            '~/Z/zhi/ebola/5km_spatial_summary_rasters/IGBP_Landcover/Majority/2012.majority.class.tif',
            '~/Dropbox/SEEG_mapping/LF_limits/data/Raster_various/AI_Bare.tif')

# give them readable names
names <- c('EVI_mean',
           'EVI_range',
           'LSTday_mean',
           'LSTday_range',
           'LSTnight_mean',
           'LSTnight_range',
           'precip_mean',
           'precip_annamp',
           'access_50k',
           'afripop',
           'PET_mean',
           'DEM',
           'TCW_mean',
           'TCB_mean',
           'LC_class',
           'arid_areas')

# loop through opening links to the rasters
covs <- lapply(covars, raster)

# crop the final raster (a global map) to the same extent as the others
covs[[which(names == 'arid_areas')]] <- crop(covs[[which(names == 'arid_areas')]],
                              covs[[1]])

# invert the arid areas so that 1 is arid
covs[[which(names == 'arid_areas')]] <- 1 - covs[[which(names == 'arid_areas')]]

# stack the rasters
covs <- stack(covs)

# give them nicer names
names(covs) <- names

# set the NA value
NAvalue(covs) <- -9999

# set the minimum value of population to a very small *positive* number
covs$afripop[getValues(covs$afripop) < 0.001] <- 0.001

# # ~~~~~~~~~~~~~~
# # create a mask layer to remove seas and lakes
# 
# # start with the EVI layer
# mask <- covs[[1]]
# 
# # load a shapefile of the large waterbodies in Africa
# inland_water <- shapefile('~/Dropbox/HAT/data/covariates/water/shapefiles/inwatera.shp')
# 
# plot(inland_water, add = TRUE)
#   raster('~/Dropbox/HAT/data/covariates/water/water_perm.grd')
# 
# # extend it by 1 pixel to match the layout
# waterbodies <- extend(waterbodies, mask)
# 
# # and mask it by the same layer
# waterbodies <- mask(waterbodies, mask)
# 
# # set waterbodies to NA
# NAvalue(waterbodies) <- 1
# plot(waterbodies)

# remask all of the layers against the first (EVI)
covs <- mask(covs, covs[[1]])

# output them as a multiband .grd file
writeRaster(covs,
            file = '~/tmp/5km/covs',
            overwrite = TRUE)
