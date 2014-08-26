# fit an ebola niche model

# clear workspace
rm(list = ls())

# set the RNG seed
set.seed(1)

# load packages
library(seegSDM)
library(snowfall)

# set output path
outpath <- 'output/poisson/'

# define functions


# load the data

# raster covariates
covs <- brick('~/tmp/10km/covs.grd')
# covs <- importRasters('~/tmp/',
#                       as = stack,
#                       ext = 'tif')


# which layer gives population?
pop_idx <- which(names(covs) == "afripop_1km")
pop <- covs[[pop_idx]]

covs <- covs[[-pop_idx]]

# occurrence data
occ <- read.csv('data/occurrence/clean/occurrence.csv')

# remove Marburg data
occ <- occ[occ$virus != 'Marburg', ]

# generate pseudo-absence data uniformly at random from across the entire grid
bg <- bgSample(pop,
               n = 5000,
               prob = FALSE,
               replace = FALSE,
               spatial = FALSE)

colnames(bg) <- c('long', 'lat')

dat <- rbind(cbind(PA = rep(1, nrow(occ)),
                   occ[, c('long', 'lat')]),
             cbind(PA = rep(0, nrow(bg)),
                   bg))

# extract covariates for all points
dat_covs <- extract(covs, dat[, -1])

# combine covariates with the other info
dat_all <- cbind(dat, dat_covs)

# extract the human population for each of these points
dat_pop <- extract(pop, dat[, -1])

# NO OUTBREAKS ARE IN UNPOPULATED AREAS!!
sum(dat[dat_pop == 0, 1])

# plot(pop, maxpixels = 100000)
# 
# points(lat ~ long, data = dat[dat$PA == 1, ])
# points(lat ~ long,
#        data = dat[dat$PA == 1 & dat_pop == 0, ],
#        pch = 16)

# remove all records in uninhabited areas
# empty <- dat_pop == 0
# dat_pop <- dat_pop[!empty]
# dat_all <- dat_all[!empty, ]

# set unpopulated areas to 0.01 to avoid divide by 0 errors
dat_pop[dat_pop == 0] <- 0.01

# add this as an offset column to the dataframe, giving the number
# of person years in that spot.
dat_all <- cbind(dat_all,
                 off = log(38 * dat_pop))

ncpu <- 50
nboot <- ncpu * 1

# get random bootstraps of the data (minimum 5 pres/5 abs)
data_list <- replicate(nboot,
                       subsample(dat_all,
                                 nrow(dat_all),
                                 minimum = c(10, 10)),
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
                       method = 'perf',
                       n.trees = 5000,
                       family = 'poisson',
                       gbm.offset = ncol(dat_all))

# pred_person <- model_list[[1]]$pred
# pred_pixel <- pred_person * pop
# 
# plot(pred_person)
# 
# plot(log(pred_pixel))
# 
# # number of outbreaks expected since 1976
# sum(pred_pixel[],
#     na.rm = TRUE) * 38


# stop the cluster
sfStop()

# summarise all the ensembles
preds <- stack(lapply(model_list, '[[', 4))
preds_sry <- combinePreds(preds)
names(preds_sry) <- c('mean',
                      'median',
                      'lowerCI',
                      'upperCI')

# save the prediction summary
writeRaster(preds_sry,
            file = paste0(outpath,
                          'prediction'),
            format = 'GTiff',
            overwrite = TRUE)

# save the relative influence scores
relinf <- getRelInf(model_list)
write.csv(relinf,
          file = paste0(outpath,
                        'relative_influence.csv'))

# plot and the marginal effect curves
png(paste0(outpath,
           'effects.png'),
    width = 2000,
    height = 2500,
    pointsize = 30)

par(mfrow = n2mfrow(nlayers(covs)))
effects <- getEffectPlots(model_list,
                          plot = TRUE)

dev.off()

# plot the risk map
png(paste0(outpath,
           'risk.png'),
    width = 2000,
    height = 2000,
    pointsize = 30)

par(oma = rep(0, 4),
    mar = c(0, 0, 0, 2))

plot(preds_sry[[1]],
     axes = FALSE,
     box = FALSE)

points(dat[dat$PA == 1, -1],
       pch = 16,
       cex = 1,
       col = 'blue')

dev.off()