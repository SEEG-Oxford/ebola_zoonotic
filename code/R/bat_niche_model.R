# run bat niche models

# clear workspace
rm(list = ls())

# set the RNG seed
set.seed(1)

# load packages
library(seegSDM)
library(snowfall)

# set output path
outpath <- 'output/bats/'

# load the data

# raster covariates
covs <- brick('~/tmp/5km/covs.grd')

# remove access, land cover class, tasseled-cap indices and arid areas
# also remove precipitation as it so spatially coarse and population
bad_covs <- c('access_50k',
              'LC_class',
              'TCW_mean',
              'TCB_mean',
              'arid_areas',
              'precip_mean',
              'precip_annamp',
              'afripop')

covs <- covs[[which(!(names(covs) %in% bad_covs))]]

# occurrence data
all_bats <- read.csv('data/bats/clean/megabat_occurrence.csv')

# load the names of key species
key_species <- read.csv('data/bats/key_species.csv',
                        stringsAsFactors = FALSE)[, 1]

# loop through these key species fitting models
for (species in key_species) {
  
  # get version of species name with underscores for spaces
  species_ <- gsub(' ',
                   '_',
                   species)
  
  # set output path
  outpath_species <- paste0(outpath,
                            species_,
                            '/')
  
  # create the directory if it doesn't already exist
  if (!file.exists(outpath_species)) {
    dir.create(outpath_species)
  }
  
  # get presence/background dataset
  dat <- cbind(PA = as.numeric(all_bats$species == species),
               all_bats[, 3:2])
  
  # extract covariates for all points
  dat_covs <- extract(covs, dat[, -1])
  
  # combine covariates with the other info
  dat_all <- cbind(dat, dat_covs)
  
  ncpu <- 50
  nboot <- ncpu * 1
  
  # get random bootstraps of the data (minimum 5 pres/5 abs)
  data_list <- replicate(nboot,
                         subsample(dat_all,
                                   nrow(dat_all),
                                   minimum = c(5, 5)),
                         simplify = FALSE)
  
  # initialize the cluster
  sfInit(parallel = TRUE, cpus = ncpu)
  sfLibrary(seegSDM)
  
  model_list <- sfLapply(data_list,
                         runBRT,
                         gbm.x = 4:(ncol(dat_all) - 1),
                         gbm.y = 1,
                         pred.raster = covs,
                         gbm.coords = 2:3,
                         wt = function(PA) ifelse(PA == 1, 1, sum(PA) / sum(1 - PA)))

  # get cv statistics in parallel
  stat_lis <- sfLapply(model_list, getStats)
  
  # summarise all the ensembles
  preds <- stack(lapply(model_list, '[[', 4))
  
  # summarise the predictions in parallel
  preds_sry <- combinePreds(preds, parallel = TRUE)
  
  # stop the cluster
  sfStop()
    
  # convert the stats list into a matrix using the do.call function
  stats <- do.call("rbind", stat_lis)
  
  # save them
  write.csv(stats,
            paste0(outpath_species,
                   '/stats.csv'))

  names(preds_sry) <- c('mean',
                        'median',
                        'lowerCI',
                        'upperCI')
  
  # save the prediction summary
  writeRaster(preds_sry,
              file = paste0(outpath_species,
                            species_),
              format = 'GTiff',
              overwrite = TRUE)
  
  # save the relative influence scores
  relinf <- getRelInf(model_list)
  
  write.csv(relinf,
            file = paste0(outpath_species,
                          'relative_influence.csv'))
  
  # plot and the marginal effect curves
  png(paste0(outpath_species,
             'effects.png'),
      width = 2000,
      height = 2500,
      pointsize = 30)
  
  par(mfrow = n2mfrow(nlayers(covs)))
  
  effects <- getEffectPlots(model_list,
                            plot = TRUE)
  
  dev.off()
  
  # plot the risk map
  png(paste0(outpath_species,
             species_,
             '.png'),
      width = 2000,
      height = 2000,
      pointsize = 30)
  
  par(oma = rep(0, 4),
      mar = c(0, 0, 0, 2))
  
  plot(preds_sry[[1]],
       axes = FALSE,
       box = FALSE)
  
  points(dat[dat$PA == 1, 2:3],
         pch = 16,
         cex = 1,
         col = 'blue')
  
  dev.off()
  
}