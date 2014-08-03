# create a raster layer of the density of mega bats

# clear workspace
rm(list = ls())

# load packages
library(devtools)
install_github('taxize_',
               'ropensci')
library(seegSDM)
library(taxize) # getting genera
library(snowfall) # parallel

# ~~~~~~~~~~
# define functions 

rasterizeSpecies <- function(species, shape, raster, folder = 'mega_bats') {
  tmp <- rasterize(shape[shape$BINOMIAL == species, ],
                   raster,
                   field = 1,
                   background = 0,
                   fun = 'first')

  writeRaster(tmp,
              filename = paste0('~/tmp/',
                                folder,
                                '/',
                                gsub(' ', '_', species)),
              format = 'GTiff',
              overwrite = TRUE)
  
  rm(tmp)
  
  return (NULL)
}

# ~~~~~~~~~~
# load data

# 1km template raster layer
template <- raster('~/Z/zhi/friction_1km.tif')

# IUCN mammal range shapefiles
mammterr <- shapefile('~/Z/users/nick/MERS-CoV/bats/MAMMTERR/MAMMTERR.shp')


# use taxize to get the names of micro and mega bat genera
mega <- itis_downstream(552301,
                        'Genus') # megachiroptera
micro <- itis_downstream(552302,
                         'Genus') # microchiroptera

mega_genera <- as.character(mega$taxonname)
micro_genera <- as.character(micro$taxonname)

# get ALL genera for terrestrial mammals in shapefile
all_genera <- sapply(mammterr$BINOMIAL,
                     function(x) strsplit(x, ' ')[[1]][1])

# find genera which are mega/micro bats and keep only these
mega_idx <- which(all_genera %in% mega_genera)
micro_idx <- which(all_genera %in% micro_genera)

# get shapefiles for each group
mega_shp <- mammterr[mega_idx, ]
micro_shp<- mammterr[micro_idx, ]

# remove mammterr to free up memory
rm(mammterr)

# get the names of species for which there are shapefiles
mega_species <- unique(mega_shp$BINOMIAL)
micro_species <- unique(micro_shp$BINOMIAL)

sfInit(cpus = 60, parallel = TRUE)
sfLibrary(raster)

mega_distrib <- sfLapply(mega_species,
                         rasterizeSpecies,
                         mega_shp,
                         template)

micro_distrib <- sfLapply(micro_species,
                          rasterizeSpecies,
                          micro_shp,
                          template,
                          folder = 'micro_bats')

# loop through these, check if they contain any of the species' range,
# if they do mask and save them, else delete them

tidySpecies <- function (filename, template) {
  # load a raster if it containsany of the species' range,
  # mask and resave it, else delete it
  tmp <- raster(filename)
  if (maxValue(tmp) == 1) {
    tmp <- mask(tmp,
                template)
    
    writeRaster(tmp,
                file = filename,
                overwrite = TRUE)

    } else {
      
    rm(tmp)
    file.remove(filename)
    
  }
  
  return (NULL)
  
}

mega_files <- list.files('~/tmp/mega_bats/',
                         full.names = TRUE)

micro_files <- list.files('~/tmp/micro_bats/',
                          full.names = TRUE)

sfLapply(mega_files, tidySpecies, template)

sfLapply(micro_files, tidySpecies, template)

sfStop()
