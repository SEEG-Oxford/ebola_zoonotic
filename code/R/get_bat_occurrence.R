# get mega bat occurrence data from GBIF

# clear workspace
rm(list = ls())

# load packages
library(seegSDM)
library(taxize)

# load a template raster
template <- raster('~/tmp/5km/covs.grd')

# get the extent of interest
ext <- extent(template)

# and remove the raster again
rm(template)

# read in all the genera for the mega and microbats
mega_genera <- read.csv('data/bats/megabat_genera.csv',
                        stringsAsFactors = FALSE)[, 1]

key_species <- read.csv('data/bats/key_species.csv',
                         stringsAsFactors = FALSE)[, 1]


# if it hasn't already been done, 
if (!file.exists('data/bats/raw/megabat_occurrence.csv')) {
  
  # get occurrence points for all the megabats
  mega_list <- lapply(mega_genera,
                      species = '*',
                      gbif,
                      ext = ext)
  
  mega_df <- do.call(rbind,
                     mega_list)
  
  write.csv(mega_df,
            file = 'data/bats/raw/megabat_occurrence.csv',
            row.names = FALSE)
} else {
  
 mega_df <- read.csv('data/bats/raw/megabat_occurrence.csv')
 
}

# get the species in Africa
mapped_species <- read.csv('~/Z/zhi/ebola/bats/mega_bats_buff_tif_layernames.csv')[, 1]
mapped_species <- gsub('_',
                       ' ',
                       mapped_species)
# 
# # access all the buffered range maps
# all_ranges <- brick('~/Z/zhi/ebola/bats/mega_bats_buff.tif')
# 
# # plot the distributions
# for (i in 1:length(mapped_species)) {
#   
#   species <- mapped_species[i]
#   # get name with underscore instead of a space
#   species_ <- gsub(' ',
#                    '_',
#                    species)
#   
#   # load EO range map
#   range <- all_ranges[[i]]
#   
#   # set up plot
#   png(paste0('output/bats/all/',
#              species_,
#              '.png'),
#       width = 2000,
#       height = 2500,
#       pointsize = 40)
#   
#   par(mar = rep(0, 4))
#   
#   image(range,
#         maxpixels = 1000000,
#         asp = 1,
#        axes = FALSE,
#        col = c('light grey',
#                'dark grey'))
#   
#   occ <- mega_df[mega_df$species == species, ]
#   
#   mtext(text = paste(species,
#                      nrow(occ),
#                      sep = ' n = '),
#         side = 1,
#         line = -3,
#         cex = 1.8)
#   
#   points(lat ~ lon,
#          data = mega_df[mega_df$species == species, ],
#          pch = 16,
#          cex = 0.8,
#          col = rgb(0.2, 0.2, 0.8))
# 
#   dev.off()
#   
# }