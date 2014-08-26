# clean megabat occurrence data

# clear workspace
rm(list = ls())

# load packages
library(raster)

# load functions
source('code/R/functions.R')

# load a template raster
template <- raster('~/tmp/5km/covs.grd')

# load bat occurrence data
bats <- read.csv('data/bats/raw/megabat_occurrence.csv')

# remove any with missing coordinates
missing <- is.na(bats$lat) | is.na(bats$lon)
bats <- bats[!missing, ]

# subset to only first three names
bats$species <- firstTwo(bats$species)

# set all species names to sentence case
bats$species <- sentenceCase(bats$species)

# find names with question marks and remove them
weird_bats <- grep('\\?', bats$species)
bats <- bats[-weird_bats, ]

# keep only species name and lat/long
bats <- bats[, c('species', 'lat', 'lon')] 

# remove duplicate records
bats <- bats[!duplicated(bats), ]

# remove records within a 4x4 degree square around the lat/lon origin
near_origin <- which(abs(bats$lat - 0) < 2 & abs(bats$lon - 0) < 2)
bats <- bats[-near_origin, ]

# ~~~~~~~~~~~~~
# exclude occurrences over 100km from the IUCN EO range maps

# load the buffered IUCN range maps
range_buff <- brick('~/Z/zhi/ebola/bats/mega_bats_buff.tif')
range_buff_names <- read.csv('~/Z/zhi/ebola/bats/mega_bats_buff_tif_layernames.csv')[, 1]


for (species in unique(bats$species)) {
  
  # get species name with an underscore
  species_ <- gsub(' ', '_', species)
  
  # get the index for these records
  species_idx <- which(bats$species == species)
  
  # if there's an african range map for it
  if (any(range_buff_names == species_)) {
    
    # get the range map for this species
    range <- range_buff[[which(range_buff_names == species_)]]
    
    # extract the range values for the occurrence points
    # (1 is in, 0 is out)
    in_out <- extract(range,
                      bats[species_idx, c('lon', 'lat')])
    
    # find those in the index to chuck
    to_chuck <- is.na(in_out) | in_out == 0
    
    # if there were any to remove, do so
    if (any(to_chuck)) {
      # remove these records form bats
      bats <- bats[-species_idx[to_chuck], ]
    }
    
  } else {

    # otherwise, if there's no range in Africa remove all the records
    bats <- bats[-species_idx, ]
    
  }

}

# output data
write.csv(bats,
          file = 'data/bats/clean/megabat_occurrence.csv',
          row.names = FALSE)
