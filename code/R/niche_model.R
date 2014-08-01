# fit an ebola niche model

# clear workspace
rm(list = ls())

# set the RNG seed
set.seed(1)

# load packages
library(seegSDM)
library(snowfall)

# define functions


# load the data

# raster covariates
covs <- importRasters('data/covs/',
                      as = stack)

pop <- covs[[which(names(covs) == "afripop_._asiapop_5km")]]

# occurrence data
occ <- read.csv('data/occurrence/clean/occurrence.csv')

# remove Marburg data
occ <- occ[occ$virus != 'Marburg', ]

# generate pseudo-absence data according to the population surface
# (assume perfect detection of human cases, so reported risk only
# a function of population density)

bg <- bgSample(pop,
               n = 1000,
               prob = TRUE,
               replace = TRUE,
               spatial = FALSE)

colnames(bg) <- c('long', 'lat')

dat <- rbind(cbind(PA = rep(1, nrow(occ)),
                   occ[, c('long', 'lat')]),
             cbind(PA = rep(0, nrow(bg)),
                   bg))

dat_covs <- extract(covs, dat[, -1])

dat_all <- cbind(dat, dat_covs)

ncpu <- 60
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
                       gbm.x = 4:ncol(dat_all),
                       gbm.y = 1,
                       pred.raster = covs,
                       gbm.coords = 2:3,
                       wt = function(PA) ifelse(PA == 1, 1, sum(PA) / sum(1 - PA)))

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
            file = 'output/prediction',
            format = 'GTiff')

# save the relative influence scores
relinf <- getRelInf(model_list)
write.csv(relinf,
          file = 'output/relative_influence.csv')

# plot and the marginal effect curves
png('output/effects.png',
    width = 2000,
    height = 2500,
    pointsize = 30)

par(mfrow = n2mfrow(nlayers(covs)))
effects <- getEffectPlots(model_list,
                          plot = TRUE)

dev.off()

# plot the risk map
png('output/risk.png',
    width = 2000,
    height = 2000,
    pointsize = 30)

par(oma = rep(0, 4),
    mar = c(0, 0, 0, 2))

plot(preds_sry[[1]],
     axes = FALSE,
     box = FALSE)

points(dat[, -1],
       pch = 16,
       cex = 0.5,
       col = rgb(0.4, 0.4, 0.4, 0.3))

points(dat[dat$PA == 1, -1],
       pch = 16,
       cex = 1,
       col = 'blue')

dev.off()