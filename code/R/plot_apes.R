# plotting existing great ape data from IUCN and GBIF

# clear workspace
rm(list = ls())

# load packages
library(seegSDM)
library(taxize)

# load functions
source('code/R/functions.R')

# load a template raster
template <- raster('~/Z/zhi/afripop_1km.tif')

# get the extent of interest
ext <- extent(template)

# load entire mammals of the world shapefile
mammterr <- shapefile('~/Z/users/nick/MERS-CoV/bats/MAMMTERR/MAMMTERR.shp')

# split out shapefiles for the four species of interest
Pt <- mammterr[mammterr$BINOMIAL == 'Pan troglodytes', ]
Pp <- mammterr[mammterr$BINOMIAL == 'Pan paniscus', ]
Gg <- mammterr[mammterr$BINOMIAL == 'Gorilla gorilla', ]
Gb <- mammterr[mammterr$BINOMIAL == 'Gorilla beringei', ]

# save these to disk
shapefile(Pt,
          filename = 'data/apes/pan_troglodytes.shp',
          overwrite = TRUE)

shapefile(Pp,
          filename = 'data/apes/pan_paniscus.shp',
          overwrite = TRUE)

shapefile(Gg,
          filename = 'data/apes/gorilla_gorilla.shp',
          overwrite = TRUE)

shapefile(Gb,
          filename = 'data/apes/gorilla_beringei.shp',
          overwrite = TRUE)

# ~~~~~~~~
# genus Pan (chimps & bonobos)

# get all GBIF occurrences
pan <- gbif('Pan',
            species = '*',
            ext = ext)

# tidy names
pan$species <- firstTwo(pan$species)
pan$species <- sentenceCase(pan$species)

# get only species name and location
pan <- pan[, c('species', 'lat', 'lon')]

# remove duplicate records
pan <- pan[!duplicated(pan), ]

# remove records within a 4x4 degree square around the lat/lon origin
near_origin <- which(abs(pan$lat - 0) < 2 & abs(pan$lon - 0) < 2)
pan <- pan[-near_origin, ]

summary(factor(pan$species))

# ~~~~~~~~
# genus Gorilla (western and eastern gorillas)

# get all GBIF occurrences
gorilla <- gbif('Gorilla',
                species = '*',
                ext = ext)

# tidy names
gorilla$species <- firstTwo(gorilla$species)
gorilla$species <- sentenceCase(gorilla$species)

# get only species name and location
gorilla <- gorilla[, c('species', 'lat', 'lon')]

# remove duplicate records
gorilla <- gorilla[!duplicated(gorilla), ]

# remove records within a 4x4 degree square around the lat/lon origin
near_origin <- which(abs(gorilla$lat - 0) < 2 & abs(gorilla$lon - 0) < 2)
gorilla <- gorilla[-near_origin, ]

summary(factor(gorilla$species))

# plot all
for (species in c('Pan troglodytes',
                  'Pan paniscus',
                  'Gorilla gorilla',
                  'Gorilla beringei')) {
  
  # get species with an underscore
  species_ <- gsub(' ',
                   '_',
                   species)
  outpath <- paste0('output/apes/',
                    species_,
                    '.png')
  
  # set up a plot
  png(outpath,
      width = 2000,
      height = 2500,
      pointsize = 40)
  
  
  dat <- switch(species,
                'Pan troglodytes' = pan,
                'Pan paniscus' = pan,
                'Gorilla gorilla' = gorilla,
                'Gorilla beringei' = gorilla)
  
  # plot range and points for these species
  image(template,
        asp = 1,
        col = 'grey',
        maxpixels = 1000000,
        axes = FALSE,
        ylab = '',
        xlab = '')
  
  mtext(text = paste(species,
                     nrow(dat[dat$species == species, ]),
                     sep = ' n = '),
        side = 1,
        line = -3,
        cex = 1.8)
  
  shp <- switch(species,
                'Pan troglodytes' = Pt,
                'Pan paniscus' = Pp,
                'Gorilla gorilla' = Gg,
                'Gorilla beringei' = Gb)
  
  plot(shp,
       add = TRUE,
       col = 'dark grey',
       lty = 0)
  
  points(lat ~ lon,
         data = dat[dat$species == species, ],
         pch = 16,
         cex = 0.8,
         col = rgb(0.2, 0.2, 0.8))
  
  dev.off()
}
