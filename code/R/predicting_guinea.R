# assess predictive ability of non-Guinea model vs the Guinea index case

# clear workspace
rm(list = ls())

# load packages
library(seegSDM)
library(ggmap)

# ~~~~~~~~~~~~~~
# load data

# cleaned occurrence_data
occ <- read.csv('data/occurrence/clean/occurrence.csv')

# load mean prediction from model trained without the Guinea data
pred <- raster('output/bernoulli/drop_current/EBOV.tif')

# ~~~~~~~~~~~~
# split datasets

# Guinea is occurrence 30, get human, point datasets with and without it
Guinea <- occ[occ$outbreak_id == '30' &
                !occ$poly &
                !occ$animal, ]

not_Guinea <- occ[occ$outbreak_id != '30' &
                    !occ$poly &
                    !occ$animal, ]


# ~~~~~~~~~~~
# plot things

plot(pred)

points(lat ~ long,
       data = not_Guinea,
       pch = 16, col = grey(0.4))

points(lat ~ long,
       data = Guinea,
       pch = 16)


# get the minimum risk value of pre-Guinea index cases
thresh_100 <- min(extract(pred, not_Guinea[, c('long', 'lat')]))

risk <- pred > thresh_100

extent <- c(Guinea[, 'long'] + c(-1, 1) * range,
            Guinea[, 'lat'] + c(-1, 1) * range)

plot(risk, ext = extent)
points(Guinea[, c('long', 'lat')],
       pch = 16)
