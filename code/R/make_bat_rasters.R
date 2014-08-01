# create a raster layer of the density of mega bats

# clear workspace
rm(list = ls())

# load packages
library(devtools)
install_github('taxize_',
               'ropensci')
library(seegSDM)
library(taxize) # getting genera
library(rgeos) # gOverlaps
library(snowfall) # parallel
library(maptools) # unionSP

# ~~~~~~~~~~
# define functions 

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

sfInit(cpus = 8, parallel = TRUE)
sfLibrary(raster)

rasterize_species <- function(species, shape, raster) {
  rasterize(shape[shape$BINOMIAL == species, ],
            raster,
            field = 1,
            background = 0,
            fun = 'first')
}

mega_distib <- sfLapply(mega_species,
                        rasterize_species,
                        mega_shp,
                        template)

micro_distib <- sfLapply(micro_species,
                         rasterize_species,
                         micro_shp,
                         template)

sfStop()
