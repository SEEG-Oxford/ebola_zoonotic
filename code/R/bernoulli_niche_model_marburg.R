# fit an ebola niche model

# clear workspace
rm(list = ls())

# set the RNG seed
set.seed(1)

# load packages
library(seegSDM)
library(snowfall)

# load functions file
source('code/R/functions.R')

# set output path
outpath <- 'output/bernoulli_marburg/'

# ~~~~~~~
# model control

# whether to drop current outbreak
drop_current <- FALSE

# whether to drop non-human occurrence data
drop_animal <- FALSE

# whether to include primary bat species
drop_bats <- FALSE

# make sure there's at most only one option switched on
if (sum(drop_current, drop_animal, drop_bats) > 1) {
  stop ('only one at a time please')
}

# if dropping current outbreak, append to the output path
if (drop_current) {
  outpath <- paste0(outpath, 'drop_current/')
}

if (drop_animal) {
  outpath <- paste0(outpath, 'drop_animal/')
}

if (drop_bats) {
  outpath <- paste0(outpath, 'drop_bats/')
}

# make sure the directory exists
if (!file.exists(outpath)) {
  dir.create(outpath)
}

# ~~~~~~~~~~
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

# get the population raster
pop <- covs[[which(names(covs) == 'afripop')]]

# chuck out the bad covariates
covs <- covs[[which(!(names(covs) %in% bad_covs))]]

# add bat covariates unless dropped
if (!drop_bats) {
  bats <- raster('output/bats/primary_mean.tif')
  covs <- brick(list(covs,
                     bat_distribution = bats))
}

# occurrence data
occ <- read.csv('data/occurrence/clean/occurrence.csv')

# remove everything except Marburg data
occ <- occ[occ$virus == 'Marburg', ]

# if dropping current outbreak, remove all with outbreak ID 30
if (drop_current) {
  occ <- occ[occ$outbreak_id != 30, ]
}

if (drop_animal) {
  occ <- occ[!occ$animal, ]
}

# generate pseudo-absence data according to the population surface
# (assume perfect detection of human cases, so reported risk only
# a function of population density)

bg <- bgSample(pop,
               n = 10000,
               prob = TRUE,
               replace = TRUE,
               spatial = FALSE)

colnames(bg) <- c('long', 'lat')
bg <- data.frame(bg)

# add an outbreak id to this
bg$outbreak_id <- 0

# combine the occurrence and background records
dat <- rbind(cbind(PA = rep(1, nrow(occ)),
                   occ[, c('long', 'lat', 'outbreak_id')]),
             cbind(PA = rep(0, nrow(bg)),
                   bg))

# get the covariate values
dat_covs <- extract(covs, dat[, 2:3])

# and add them
dat_all <- cbind(dat, dat_covs)

# remove NAs
dat_all <- na.omit(dat_all)

ncpu <- 50
nboot <- ncpu * 10

# create a list with random permutations of dat_all, sampling one occurrence
# from each polygon in each iteration, the bootstrapping as usual.
# This way there's more jitter and it avoids the horrible normalization in
# gbm which otherwise gives some points a weight of ~250 (!).
data_list <- replicate(nboot,
                       subsamplePolys(dat_all,
                                      minimum = c(30, 30)),
                       simplify = FALSE)

# initialize the cluster
sfInit(parallel = TRUE, cpus = ncpu)
sfLibrary(seegSDM)

model_list <- sfLapply(data_list,
                       runBRT,
                       gbm.x = 4:ncol(data_list[[1]]),
                       gbm.y = 1,
                       pred.raster = covs,
                       gbm.coords = 2:3,
                       wt = function(PA) ifelse(PA == 1, 1, sum(PA) / sum(1 - PA)))

# get cv statistics in parallel
stat_lis <- sfLapply(model_list, getStats)

# summarise all the ensembles
preds <- stack(lapply(model_list, '[[', 4))

# summarise the predictions in parallel
preds_sry <- combinePreds(preds)

# stop the cluster
sfStop()

# convert the stats list into a matrix using the do.call function
stats <- do.call("rbind", stat_lis)

# save them
write.csv(stats,
          paste0(outpath, '/stats.csv'))

names(preds_sry) <- c('mean',
                      'median',
                      'lowerCI',
                      'upperCI')

# save the prediction summary
writeRaster(preds_sry,
            file = paste0(outpath,
                          'EBOV'),
            format = 'GTiff',
            overwrite = TRUE)

# save the relative influence scores
relinf <- getRelInf(model_list)
write.csv(relinf,
          file = paste0(outpath,
                        'relative_influence.csv'))


# plot the risk map
png(paste0(outpath,
           'EBOV.png'),
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


# ~~~~~~
# plot marginal effect curves

# get the order of plots (all except relinf)!
order <- match(rownames(relinf), names(covs))

# set up x axis labels and titles
short_names <- c(
  'EVI mean',
  'EVI range',
  'LST mean (day)',
  'LST range (day)',
  'LST mean (night)',
  'LST range (night)',
  'PET mean',
  'Elevation',
  'Bat distribution')

units <- c(
  'index',
  'index',
  'degrees celsius',
  'degrees celsius',
  'degrees celsius',
  'degrees celsius',
  'millimetres/month',
  'metres',
  'suitability index')


effect <- getEffectPlots(model_list)

# set up device
png(paste0(outpath,
           'effects.png'),
    width = 3000,
    height = 3000,
    pointsize = 60)

# set up multi panels and margins
par(mfrow = c(3, 3),
    mar = c(5, 2, 4, 2) + 0.1,
    oma = c(0, 3, 0, 0))

# loop through plots
for (i in 1:length(effect)) {
  
  # extract summary stats
  df <- effect[[order[i]]][, 1:4]
  
  # pick y axis
  if (i %% 3 == 1) {
    ylab = 'marginal effect'
  } else {
    ylab = ''
  }
  
  
  # set up empty plotting region
  plot(df[, 2] ~ df[, 1],
       type = 'n',
       ylim = c(-3.5, 2.5),
       ylab = '',
       xlab = '')
  
  # add the 95% CIs
  polygon(x = c(df[, 1], rev(df[, 1])),
          y = c(df[, 3], rev(df[, 4])),
          border = NA,
          col = grey(0.7))
  
  # add the mean line
  lines(df[, 2] ~ df[, 1],
        lwd = 5,
        col = grey(0.2))
  
  # y axis lable (only on left hand column)
  title(ylab = ylab,
        cex.lab = 1.2,
        col.lab = grey(0.3),
        xpd = NA,
        line = 2.5)
  
  # x-axis label
  title(xlab = units[order[i]],
        cex.lab = 1.2,
        col.lab = grey(0.3),
        line = 2.5)
  
  # title
  title(main = short_names[order[i]],
        line = 1.5,
        cex.main = 1.2)
  
  # relative contribution inset
  mtext(text = round(relinf[i, 1] / 100, 2),
        side = 3,
        line = -2,
        adj = 0.07,
        col = grey(0.5))
  
}

dev.off()
