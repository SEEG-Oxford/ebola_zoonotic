# Threshold the EBOV risk map ensembles at different levels
# and sum up at risk population by country and by urabn and rural

# clear workspace
rm(list = ls())

# load packages
library(raster)
library(snowfall)

# load functions from function files
source('code/R/functions.R')

# load data

# get mean EBOV prediction raster
EBOV_risk <- brick('output/bernoulli/EBOV.tif')[[1]]

# all layers of the ensemble prediction
EBOV_all <- brick('~/tmp/ebola_bern_all.tif')

# load the covariate rasters
covs <- brick('~/tmp/5km/covs.grd')

# pull out the population layer
pop <- covs[[which(names(covs) == 'afripop')]]

# remove the other covariates
rm(covs)

# load an urban raster
urb <- raster('~/Z/mastergrids/cleandata/urbanareas/grumpALPHA/upr_u_5km.flt')

# and a peri-urban raster
peri <- raster('~/Z/mastergrids/cleandata/urbanareas/grumpALPHA/upr_p_5km.flt')

# crop these to match pop
urb <- crop(urb, pop)
peri <- crop(peri, pop)

# combine them to get a rural layer
tmp <- urb + peri
rural <- tmp == 0

# remove these other layers
rm(list = c('urb', 'peri', 'tmp'))

# create rural and urban population rasters
pop_urban <- pop * !rural
pop_rural <- pop * rural

# combine these
pop_all <- brick(pop,
                 pop_urban,
                 pop_rural)

names(pop_all) <- c('all', 'urban_periurban', 'rural')

# if an admin0 file doesn't already exist, make one
if (!(file.exists('~/tmp/5km/ad0.grd') & file.exists('~/tmp/5km/GAUL_lookup.csv'))) {
  
  # load an admin 0 layer shapefile
  ad0_shp <- shapefile('~/Z/admin_units/admin2013/admin2013_0.shp')
  
  # rasterize it to the same resolution and extent as the population
  # and prediction layers
  ad0_raster <- rasterize(ad0_shp,
                          pop,
                          field = 'GAUL_CODE')
  
  # save the lookup table for country names and GAUL codes
  GAUL <- ad0_shp@data[, c('NAME', 'GAUL_CODE')]
  
  # save these
  writeRaster(ad0_raster,
              file = '~/tmp/5km/ad0.grd',
              overwrite = TRUE)
  
  write.csv(GAUL,
            file = '~/tmp/5km/GAUL_lookup.csv',
            row.names = FALSE)
  
} else {
  
  # otherwise load them
  ad0_raster <- raster('~/tmp/5km/ad0.grd')
  GAUL <- read.csv('~/tmp/5km/GAUL_lookup.csv')
  
}

# load human index case occurrence data
occ <- read.csv('data/occurrence/clean/occurrence.csv')

# subset this to human cases and points
occ <- occ[!is.na(occ$animal) &
             !occ$animal &
             !occ$poly, ]

# define the core set of countries
core_set <- c("Democratic Republic of the Congo",
              "Gabon",
              "Congo",
              "Guinea",
              "Côte d'Ivoire",
              "Uganda",
              "South Sudan")

# ~~~~~~~~~~~
# classify the EBOV risk map as a presence/absence based on locations od occurrences

# threshold value which classes all occurrence records as at-risk
EBOV_pres <- thresholdRisk(EBOV_risk, occ, proportion = 1)

# multiply this by population to get populations at risk in urban/peri_urban and rural areas
EBOV_PAR <- EBOV_pres * pop_all

# give the layers their names
names(EBOV_PAR) <- c('all', 'urban_periurban', 'rural')

# get the number of pixels predicted to be at risk in each country
risk_sums <- zonal(EBOV_pres,
                   ad0_raster,
                   fun = 'sum',
                   na.rm = TRUE)

# and the corresponding population at risk
PAR_sums <- zonal(EBOV_PAR,
                  ad0_raster,
                  fun = 'sum',
                  na.rm = TRUE)

# combine the two columns dataframes, if the first columns match
if (all.equal(risk_sums[, 1], PAR_sums[, 1])) {
  dat <- data.frame(country = risk_sums[, 1],
                    risk_pixels = risk_sums[, -1],
                    round(PAR_sums[, -1]))
} else {
  
  stop ('GAUL codes do not match!')
  
}

# lookup the country names
idx <- match(dat$country, GAUL[, 2])

# replace the GAUL zones with the country names
dat$country <- GAUL[idx, 1]

# subset this to only positives
dat <- dat[dat$risk_pixels > 0, ]

# add a column denoting whether they're in the core block
# start with false
dat$has_cases <- FALSE

# find those that are in the core block
in_core <- match(core_set, dat$country)

# and set these to true
dat$has_cases[in_core] <- TRUE

# sort by PAR, then whether cases are present
dat <- dat[order(dat$all, decreasing = TRUE), ]
dat <- dat[order(dat$has_cases, decreasing = TRUE), ]

# then save
write.csv(dat,
          file = 'output/bernoulli/country_risk.csv',
          row.names = FALSE)
