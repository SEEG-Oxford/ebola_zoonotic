# create a raster layer of the density of mega bats

# clear workspace
rm(list = ls())

# load packages
library(seegSDM)
library(taxize) # getting genera
library(snowfall) # parallel
library(rgeos)

# load functions
source('code/R/functions.R')

# ~~~~~~~~~~
# load data

# 1km template raster layer
template <- raster('~/tmp/5km/covs.grd')

# IUCN mammal range shapefiles
mammterr <- shapefile('~/Z/users/nick/MERS-CoV/bats/MAMMTERR/MAMMTERR.shp')

# check whether the list of genera exists & if not re-query it
if (!file.exists('data/bats/megabat_genera.csv')) {
  
  # use taxize to get the names of the mega bat genera
  mega <- itis_downstream(552301,
                          'Genus') # megachiroptera
  
  # get genus names
  mega_genera <- as.character(mega$taxonname)
  
  # write these to disk
  write.csv(mega_genera,
            file = 'data/bats/megabat_genera.csv',
            row.names= FALSE)
  
} else {
  
  # otherwise reload it
  mega_genera <- read.csv('data/bats/megabat_genera.csv')
  
  # convert to a vector
  mega_genera <- mega_genera[, 1]
  
}

# also create and write the main bat species of interest:
key_species <- c('Hypsignathus monstrosus',
                 'Epomops franqueti',
                 'Myonycteris torquata')

write.csv(key_species,
          file = 'data/bats/key_species.csv',
          row.names = FALSE)

# get ALL genera for terrestrial mammals in shapefile
all_genera <- sapply(mammterr$BINOMIAL,
                     function(x) strsplit(x, ' ')[[1]][1])

# find genera which are mega/micro bats and keep only these
mega_idx <- which(all_genera %in% mega_genera)

# get shapefiles for each group
mega_shp <- mammterr[mega_idx, ]

# remove mammterr to free up memory
rm(mammterr)

# get the names of species for which there are shapefiles
mega_species <- unique(mega_shp$BINOMIAL)

sfInit(cpus = 60, parallel = TRUE)
# sfInit(cpus = 1, parallel = FALSE)
sfLibrary(raster)
sfLibrary(rgeos)

# loop through, rasterizing these maps with an approximate 100km buffer zone
mega_distrib <- sfLapply(mega_species,
                         rasterizeSpecies,
                         mega_shp,
                         buffer = 100,
                         raster = template,
                         folder = 'bats/range_buff/')

# loop through these, check if they contain any of the species' range,
# if they do mask and save them, else delete them
mega_files <- list.files('~/Z/zhi/ebola/bats/range_buff',
                         full.names = TRUE)

sfLapply(mega_files,
         tidySpecies,
         template)

sfStop()

# stack the megabat rasters together
mega_stack <- importRasters(path = '~/Z/zhi/ebola/bats/range_buff/',
                            as = stack,
                            ext = 'tif')

# and save them as a multiband tiff
writeRaster(mega_stack,
            '~/Z/zhi/ebola/bats/mega_bats_buff',
            format = 'GTiff',
            overwrite = TRUE)

# save the layer names with them
write.csv(names(mega_stack),
          file = '~/Z/zhi/ebola/bats/mega_bats_buff_tif_layernames.csv',
          row.names = FALSE)

# loop through these key species
for (species in key_species) {
  
  # find the corresponding layer in mega_stack
  species_idx <- match(gsub(' ',
                            '_',
                            species),
                       names(mega_stack))
  
  # extract that layer
  tmp <- mega_stack[[species_idx]]
  
  # save this as a separate raster
  writeRaster(tmp,
              filename = paste0('~/Z/zhi/ebola/bats/',
                                gsub(' ',
                                     '_',
                                     species)),
              format = 'GTiff',
              overwrite = TRUE)
  
}

# calculate megabat species richness
megabat_richness <- calc(mega_stack,
                         fun = sum)

writeRaster(megabat_richness,
            file = '~/Z/zhi/ebola/bats/megabat_richness',
            format = 'GTiff',
            overwrite = TRUE)

#