# combine bat distribution maps

# clear workspace
rm(list = ls())

# load packages
library(seegSDM)

# set the primary and secondary species
primary <- c('Myonycteris torquata',
             'Epomops franqueti',
             'Hypsignathus monstrosus')

secondary <- c('Epomophorus gambianus',
               'Nanonycteris veldkampii')

# get underscore-separated versions of these
primary_ <- gsub(' ',
                 '_',
                 primary)

secondary_ <- gsub(' ',
                 '_',
                 secondary)

# get the filenames for the mean prediction rasters for each
primary_files <- paste0('output/bats/',
                        primary_,
                        '/',
                        primary_,
                        '.tif')

secondary_files <- paste0('output/bats/',
                        secondary_,
                        '/',
                        secondary_,
                        '.tif')

primary_ras <- stack(lapply(primary_files, raster))
secondary_ras <- stack(lapply(secondary_files, raster))

or <- function (x) 1 - prod(1 - x)

primary_or <- calc(primary_ras, or)
secondary_or <- calc(secondary_ras, or)

primary_max <- calc(primary_ras, max)
secondary_max <- calc(secondary_ras, max)

primary_mean <- calc(primary_ras, mean)
secondary_mean <- calc(secondary_ras, mean)

par(mfrow = c(2, 3))
plot(primary_or)
plot(primary_max)
plot(primary_mean)
plot(secondary_or)
plot(secondary_max)
plot(secondary_mean)

# save these rasters
writeRaster(primary_mean,
            file = 'output/bats/primary_mean',
            format = 'GTiff',
            overwrite = TRUE)

writeRaster(secondary_mean,
            file = 'output/bats/secondary_mean',
            format = 'GTiff',
            overwrite = TRUE)

