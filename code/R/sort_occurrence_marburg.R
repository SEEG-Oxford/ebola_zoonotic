# clean ebola occurrence data for modelling

# clear workspace
rm(list = ls())

# load packages
library(seegSDM)
library(snowfall)

# load functions from function file
source('code/R/functions.R')

#~~~~~~~~~

# load data

# raw occurrence data
dat_human <- read.csv('data/occurrence/raw/UPDATED_LATEST_Ebola_index_outbreak_140812.csv',
                      stringsAsFactors = FALSE)

dat_animal <- read.csv('data/occurrence/raw/latest_animal_case.csv',
                       stringsAsFactors = FALSE)

# convert all column names to lowercase
colnames(dat_human) <- tolower(colnames(dat_human))

colnames(dat_animal) <- tolower(colnames(dat_animal))

# make sure the polygon identifier is common to both
colnames(dat_animal)[colnames(dat_animal) == 'area_type'] <- 'shape'

# keep only the columns common to both
cols <- intersect(colnames(dat_human), colnames(dat_animal))

dat_human <- dat_human[, cols]

dat_animal <- dat_animal[, cols]

# add a column to each to identify the animal records
dat_human$animal <- FALSE

dat_animal$animal <- TRUE

# matching shapefile for polygons
shp_human <- shapefile('~/Dropbox/SEEG_mapping/ebola/data/occurrence/raw/human_idx_polygon.shp')

shp_animal <- shapefile('~/Dropbox/SEEG_mapping/ebola/data/occurrence/raw/animal_polygon.shp')

# change columns names to be the same thing
colnames(shp_human@data) <-
  colnames(shp_animal@data) <-'outbreak_id'

# increment the animal outbreak IDs so that they're above the human ones
min <- max(dat_human$outbreak_id, shp_human@data$OUTBREAK)

dat_animal$outbreak_id <- dat_animal$outbreak_id + min

shp_animal@data$outbreak_id <- shp_animal@data$outbreak_id + min

# combine the animal and human data
dat <- rbind(dat_human, dat_animal)

# set the polygon IDs to be differnt from those for human
shp_animal <- makeUniform(shp_animal)

shp <- rbind(shp_human, shp_animal)

# ~~~~~~~~~~~~~~
# tidy up the layers
# convert start and end dates into individual columns
dat$start_date <- firstDay(dat$year.start,
                           dat$month.start)

# set up a date vector and populate only the non-missing elements
dat$end_date <- rep(NA, nrow(dat))
class(dat$end_date) <- 'Date'

missing <- is.na(dat$month.end) | is.na(dat$year.end)

dat$end_date[!missing] <- lastDay(dat$year.end[!missing],
                                  dat$month.end[!missing])

dat$poly <- dat$shape == 'polygon'

# keep only the ID, virus, lat, long and start/end dates
dat <- dat[, c('outbreak_id',
               'virus',
               'lat',
               'long',
               'start_date',
               'end_date',
               'poly',
               'animal')]

# ~~~~~~~~~~~
# sample random points from within polygons on a 5km grid

# load a 5km template raster
template <- raster('~/tmp/5km/covs.grd')

# set up a cluster
sfInit(cpus = 8, parallel = TRUE)
sfLibrary(seegSDM)

# in parallel, loop through and get coordinates of all the cells covered
pt_list <- sfLapply(1:nrow(shp),
                    function (i, shp, template) {
                      
                      # get the polygon
                      poly <- shp[i, ]
                      
                      # buffer it to make sure it covers the centre of at least one pixel
                      
                      # distance from the corner to the centre of a 5km pixel,
                      # in metres, and then some
                      d <- sqrt(2 * 2500 ^ 2) + 1
                      
                      # convert to decimal degrees (at equator; hacky)
                      d <- d * 10 ^ -5
                      
                      poly <- gBuffer(poly, width = d)
                      
                      # rasterize the layer
                      tmp <- rasterize(poly,
                                       template)
                      
                      # get coordinates of the cells there
                      pts <- xyFromCell(tmp,
                                        which(!is.na(getValues(tmp))))
                      
                      return (pts)
                    },
                    shp,
                    template)

# stop the cluster
sfStop()

# split out the polygons
dat_poly <- dat[dat$poly, ]

# and the points
dat_pt <- dat[!dat$poly, ]

dat_new <- dat_pt

# add a weights column
dat_new$wt <- rep(1, nrow(dat_new))

# loop through each polygon (in the shapefile) and add it to dat
for(i in 1:nrow(shp)) {
  
  # get the ID
  ID <- shp@data$outbreak_id[i]
  
  # the points
  pts <- pt_list[[i]]
  
  # the number of points
  n <- nrow(pts)
  
  # if it's larger than 100, pick 100 of them at random
  if (n > 100) {
    pts <- pts[sample(1:100, replace = FALSE), ]
    n <- 100
  }
  
  if (n == 0) {
    break
  }
  
  # the column info
  info <- dat_poly[dat_poly$outbreak_id == ID, ]
  
  # repeat it n times
  info_mat <- info[rep(1:nrow(info), each = n),] 
  
  # add the weights
  info_mat$wt <- 1 / n
  
  # stick the coordinates in
  info_mat[, c('lat', 'long')] <- pts[, 2:1]
  
  # append it to dat
  dat_new <- rbind(dat_new, info_mat)
}


plot(template, maxpixels = 100000)
points(lat ~ long,
       data = dat_new,
       pch = 16,
       cex = 0.8,
       col = rgb(0.4, 0.4, 0.4))


# if the resulting dataset looks fairly clean, write it to disk
if (!(any(is.na(dat_new)))) {
  
  # output the resulting table
  write.csv(dat_new,
            file = 'data/occurrence/clean/occurrence.csv',
            row.names = FALSE)
  
} else {
  
  # otherwise throw an error
  stop ('missing values in the dataset!')
  
}