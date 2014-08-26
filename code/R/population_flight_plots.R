# PAR by country stacked bar plot, changing travel volume etc.

# clear workspace
rm(list = ls())

# load packages
library(RColorBrewer)

# load data

# PAR estimates by country
PAR <- read.csv('output/bernoulli/country_risk.csv',
                stringsAsFactors = FALSE)

# population estimates by country
pop <- read.csv('data/population_flights/ebola_countries_pop.csv',
                stringsAsFactors = FALSE)

# passenger numbers by country
pass <- read.csv('data/population_flights/passengers.csv',
                 stringsAsFactors = FALSE)

# seat capacity by country
cap <- read.csv('data/population_flights/capacity.csv',
                 stringsAsFactors = FALSE)

# change column names from volumes to passengers
colnames(pass) <- gsub('volume',
                       'passengers',
                       colnames(pass))

colnames(cap) <- gsub('volume',
                       'capacity',
                       colnames(cap))

# fix cote d'ivoire
pop$Country.Name[grep('Ivoire', pop$Country.Name)] <- "Côte d'Ivoire"
pass$country[grep('Ivoire', pass$country)] <- "Côte d'Ivoire"
cap$country[grep('Ivoire', cap$country)] <- "Côte d'Ivoire"

# Tanzania
PAR$country[grep('Tanzania', PAR$country)] <- 'Tanzania'
pass$country[grep('Tanzania', pass$country)] <- 'Tanzania'
cap$country[grep('Tanzania', cap$country)] <- 'Tanzania'

# remove excess countries
pop <- pop[pop$Country.Name %in% PAR$country, ]
pass <- pass[pass$country %in% PAR$country, ]
cap <-cap[cap$country %in% PAR$country, ]

# match up names
pop <- pop[match(PAR$country, pop$Country.Name),]
pass <- pass[match(PAR$country, pass$country),]
cap <- cap[match(PAR$country, cap$country),]

# combine the three dataframes
dat <- cbind(PAR, pop[, -1], pass[, -1], cap[, -1])

# remove South Sudan flight numbers before 2011
dat[dat$country == 'South Sudan', c(paste0('passengers_', 2005:2011),
                                    paste0('capacity_', 2000:2011))] <- 0

# recalculate totals
dat$pop_1976 <- dat$U1976 + dat$R1976
dat$pop_2000 <- dat$U2000 + dat$R2000
dat$pop_2013 <- dat$U2013 + dat$R2013

# remove previous totals
dat <- dat[, !(colnames(dat) %in% c('X1976.total',
                                    'X2013.total',
                                    'X..change',
                                    'Country.code'))]

# rename long countries
dat$country[dat$country == 'Central African Republic'] <- 'CAR'
dat$country[dat$country == 'Democratic Republic of the Congo'] <- 'DRC'
dat$country[dat$country == 'Equatorial Guinea'] <- 'Eq. Guinea'
dat$country[dat$country == 'Congo'] <- 'ROC'

# ~~~~~~~~~~~
# bar charts by country for population at risk estimates

# define colours from map
map_red <- rgb(214, 47, 39, maxColorValue = 255)
map_red_light <- rgb(214, 127, 119, maxColorValue = 255)

tiff('shared/figs/Figure_5B_600dpi.tif',
     width = 21,
     height = 8.7,
     units = 'cm',
     res = 600,
     pointsize = 9,
     compression = 'lzw')

par(mar = c(7, 5, 4, 1))

# plot total population
coords <- barplot(dat$population_at_risk,
                  type = 'n',
                  border = NA,
                  xlab = '',
                  ylab = 'Population at risk (100,000s)',
                  ylim = c(0, 150 * 10 ^ 5),
                  axes = FALSE)

# get the range of x values for set 1
set1_x <- range(coords[1:7, 1]) + c(-0.5, 0.5) * diff(coords[1:2, 1])

# plot a rectangle
rect(-0,
     -2 * 10 ^ 5,
     set1_x[2],
     150 * 10 ^ 5,
     border = NA,
     col = grey(0.9),
     xpd = NA)

barplot(dat$population_at_risk ,
        col = rep(c(map_red, map_red_light), c(7, nrow(dat) - 7)),
        border = NA,
        axes = FALSE,
        add = TRUE)

pop_seq <- c(0, 50, 100, 150)
pop_seq_labs <- c('', as.character(pop_seq[-1]))

axis(side = 2,
     at = pop_seq * 10 ^ 5,
     labels = pop_seq_labs,
     lwd.ticks = 0,
     las = 1,
     col = grey(0.4))

axis(side = 1,
     at = coords[, 1],
     labels = dat$country,
     tick = FALSE,
     las = 2,
     col = grey(0.4))

PAR_labs <- as.character(round(dat$population_at_risk / 10 ^ 5, 1))
PAR_labs[PAR_labs == '0'] <- '<0.1'

text(x = coords[, 1],
     y = dat$population_at_risk + 5 * 10 ^ 5,
     labels = PAR_labs,
     col = grey(0.2))



dev.off()



# ~~~~~~~~~~~
# stacked bar charts by country for population estimates 1976, 2000, 2013 by Urban & rural

urban <- brewer.pal(n = 8, name = 'PuBu')[6]
urban_light <- brewer.pal(n = 8, name = 'PuBu')[5]
rural <- brewer.pal(n = 8, name = 'YlGn')[6]
rural_light <- brewer.pal(n = 8, name = 'YlGn')[5]

tiff('shared/figs/Population_600dpi.tif',
     width = 21,
     height = 8.7,
     units = 'cm',
     res = 600,
     pointsize = 9,
     compression = 'lzw')

par(mar = c(7, 5, 2, 1))

# plot total population
coords <- barplot(as.matrix(t(dat[, c('pop_1976', 'pop_2000', 'pop_2013')])),
                  beside = TRUE,
                  border = NA,
                  names.arg = rep('', nrow(dat)),
                  col = rep(c(urban, urban_light),
                            c(7 * 3, (nrow(dat) - 7) * 3)),
                  space = c(0.1, 1),
                  xlab = '',
                  ylab = 'Population (millions)',
                  ylim = c(0, 200 * 10 ^ 6),
                  axes = FALSE)

# get the range of x values for set 1
set1_x <- range(coords[1:3, 1:7]) + c(-1, 1) * diff(coords[1:2, 1])

# plot a rectangle
rect(set1_x[1],
     -2 * 10 ^ 6,
     set1_x[2],
     200 * 10 ^ 6,
     border = NA,
     col = grey(0.9),
     xpd = NA)


barplot(as.matrix(t(dat[, c('pop_1976', 'pop_2000', 'pop_2013')])),
        beside = TRUE,
        border = NA,
        names.arg = rep('', nrow(dat)),
        col = rep(c(urban, urban_light),
                  c(7 * 3, (nrow(dat) - 7) * 3)),
        space = c(0.1, 1),
        axes = FALSE,
        add = TRUE)

barplot(as.matrix(t(dat[, c('R1976', 'R2000', 'R2013')])),
        beside = TRUE,
        border = NA,
        names.arg = rep('', nrow(dat)),
        col = rep(c(rural, rural_light),
                  c(7 * 3, (nrow(dat) - 7) * 3)),
        space = c(0.1, 1),
        axes = FALSE,
        add = TRUE)

pop_seq <- c(0, 50, 100, 150, 200)
pop_seq_labs <- c('', as.character(pop_seq[-1]))

axis(side = 2,
     at = pop_seq * 10 ^ 6,
     labels = pop_seq_labs,
     lwd.ticks = 0,
     las = 1,
     col = grey(0.4))

axis(side = 1,
     at = coords[2, ],
     labels = dat$country,
     tick = FALSE,
     las = 2,
     col = grey(0.4))

change_labs <- round(dat$pop_2013 / dat$pop_1976, 2)

text(x = coords[2, ],
     adj = 0.5,
     y = pmax(dat$pop_2013, dat$pop_2000, dat$pop_1976) + 15 * 10 ^ 6,
     labels = change_labs,
     col = grey(0.2),
     cex = 0.8)



dev.off()

# ~~~~~~~
# seat capacity by country (& set)

# define colours
before <- brewer.pal(n = 8, name = 'RdPu')[6]
before_light <- brewer.pal(n = 8, name = 'RdPu')[5]
after <- brewer.pal(n = 8, name = 'BuPu')[6]
after_light <- brewer.pal(n = 8, name = 'BuPu')[5]


tiff('shared/figs/Seat_capacity_600dpi.tif',
     width = 21,
     height = 8.7,
     units = 'cm',
     res = 600,
     pointsize = 9,
     compression = 'lzw')

par(mar = c(7, 5, 2, 1))

# plot total population
coords <- barplot(as.matrix(t(dat[, c('capacity_2000', 'capacity_2013')])),
                  beside = TRUE,
                  border = NA,
                  names.arg = rep('', nrow(dat)),
                  col = c(rep(c(before,
                                after),
                              7),
                          rep(c(before_light,
                                after_light),
                              nrow(dat) - 7)),
                  space = c(0.1, 1),
                  xlab = '',
                  ylab = 'Passenger seat capacity (millions)',
                  ylim = c(0, 4.3 * 10 ^ 6),
                  axes = FALSE)

# get the range of x values for set 1
set1_x <- range(coords[1:2, 1:7]) + c(-1, 1) * diff(coords[1:2, 1])

# plot a rectangle
rect(set1_x[1],
     -0.05 * 10 ^ 6,
     set1_x[2],
     4 * 10 ^ 6,
     border = NA,
     col = grey(0.9),
     xpd = NA)


barplot(as.matrix(t(dat[, c('capacity_2000', 'capacity_2013')])),
        beside = TRUE,
        border = NA,
        names.arg = rep('', nrow(dat)),
        col = c(rep(c(before,
                      after),
                    7),
                rep(c(before_light,
                      after_light),
                    nrow(dat) - 7)),
        space = c(0.1, 1),
        axes = FALSE,
        add = TRUE)

pop_seq <- c(0, 1, 2, 3, 4)
pop_seq_labs <- c('', as.character(pop_seq[-1]))

axis(side = 2,
     at = pop_seq * 10 ^ 6,
     labels = pop_seq_labs,
     lwd.ticks = 0,
     las = 1,
     col = grey(0.4))

axis(side = 1,
     at = coords[2, ],
     labels = dat$country,
     tick = FALSE,
     las = 2,
     col = grey(0.4))

change_labs <- round(dat$capacity_2013 / dat$capacity_2000, 2)
change_labs <- ifelse(is.finite(change_labs), change_labs, '')

text(x = coords[1, ] + diff(coords[1:2, 1] / 2),
     adj = 0.5,
     y = pmax(dat$capacity_2000, dat$capacity_2013) + 0.2 * 10 ^ 6,
     labels = change_labs,
     col = grey(0.2),
     cex = 0.8)

dev.off()

# ~~~~~~~
# passenger volume by country (& set)

# define colours
before <- brewer.pal(n = 8, name = 'RdPu')[6]
before_light <- brewer.pal(n = 8, name = 'RdPu')[5]
after <- brewer.pal(n = 8, name = 'BuPu')[6]
after_light <- brewer.pal(n = 8, name = 'BuPu')[5]

tiff('shared/figs/Passengers_600dpi.tif',
     width = 21,
     height = 8.7,
     units = 'cm',
     res = 600,
     pointsize = 9,
     compression = 'lzw')

par(mar = c(7, 5, 2, 1))

# plot total population
coords <- barplot(as.matrix(t(dat[, c('passengers_2005', 'passengers_2012')])),
                  beside = TRUE,
                  border = NA,
                  names.arg = rep('', nrow(dat)),
                  col = c(rep(c(before,
                                after),
                              7),
                          rep(c(before_light,
                                after_light),
                              nrow(dat) - 7)),
                  space = c(0.1, 1),
                  xlab = '',
                  ylab = 'Passenger volume (millions)',
                  ylim = c(0, 2 * 10 ^ 6),
                  axes = FALSE)

# get the range of x values for set 1
set1_x <- range(coords[1:2, 1:7]) + c(-1, 1) * diff(coords[1:2, 1])

# plot a rectangle
rect(set1_x[1],
     -0.02 * 10 ^ 6,
     set1_x[2],
     2 * 10 ^ 6,
     border = NA,
     col = grey(0.9),
     xpd = NA)


barplot(as.matrix(t(dat[, c('passengers_2005', 'passengers_2012')])),
        beside = TRUE,
        border = NA,
        names.arg = rep('', nrow(dat)),
        col = c(rep(c(before,
                      after),
                    7),
                rep(c(before_light,
                      after_light),
                    nrow(dat) - 7)),
        space = c(0.1, 1),
        axes = FALSE,
        add = TRUE)

pop_seq <- c(0, 0.5, 1, 1.5, 2)
pop_seq_labs <- c('', as.character(pop_seq[-1]))

axis(side = 2,
     at = pop_seq * 10 ^ 6,
     labels = pop_seq_labs,
     lwd.ticks = 0,
     las = 1,
     col = grey(0.4))

axis(side = 1,
     at = coords[2, ],
     labels = dat$country,
     tick = FALSE,
     las = 2,
     col = grey(0.4))

change_labs <- round(dat$passengers_2012 / dat$passengers_2005, 2)
change_labs <- ifelse(is.finite(change_labs), change_labs, '')

text(x = coords[1, ] + diff(coords[1:2, 1] / 2),
     adj = 0.5,
     y = pmax(dat$passengers_2005, dat$passengers_2012) + 0.1 * 10 ^ 6,
     labels = change_labs,
     col = grey(0.2),
     cex = 0.8)



dev.off()

# ~~~~~~~~~~~~~~~
# flights by region

# load data
regions <- read.csv('data/population_flights/regional_arrivals.csv')

# split region names
regions$region <- gsub(' ',
                       '\n',
                       regions$region)


# split into regions
who_regions <- regions[regions$region_type == 'who', ]
wb_regions <- regions[regions$region_type == 'world_bank', ]

# drop the unknown category from wb regions
wb_regions <- wb_regions[wb_regions$region != 'Unknown', ]

# reverse order or world bank categories
wb_regions <- wb_regions[nrow(wb_regions):1, ]

# order WHO regions alphabetically by region
who_regions <- who_regions[order(who_regions$region), ]

# define colours

# World Bank regions
Low <- rgb(252, 199, 228, maxColorValue = 255)
Lower_Middle <- rgb(215, 158, 158, maxColorValue = 255)
Upper_Middle <- rgb(168, 135, 0, maxColorValue = 255)
High <- rgb(168, 112, 0, maxColorValue = 255)
# Unknown <- rgb(190, 179, 252, maxColorValue = 255)

wb_col <- c(Low,
            Lower_Middle,
            Upper_Middle,
            High)

# WHO regions  
AFRO <- rgb(202, 122, 245, maxColorValue = 255)
AMRO <- rgb(252, 219, 163, maxColorValue = 255)
EMRO <- rgb(152, 230, 0, maxColorValue = 255)
EURO <- rgb(158, 187, 215, maxColorValue = 255)
SEARO <- rgb(196, 219, 212, maxColorValue = 255)
WPRO <- rgb(252, 204, 182, maxColorValue = 255)

who_col <- c(AFRO,
             AMRO,
             EMRO,
             EURO,
             SEARO,
             WPRO)

# create slightly darker versions of these

max_out <- 300

# World Bank regions
Low2 <- rgb(252, 199, 228, maxColorValue = max_out)
Lower_Middle2 <- rgb(215, 158, 158, maxColorValue = max_out)
Upper_Middle2 <- rgb(168, 135, 0, maxColorValue = max_out)
High2 <- rgb(168, 112, 0, maxColorValue = max_out)
# Unknown2 <- rgb(190, 179, 252, maxColorValue = max_out)

wb_col2 <- c(Low2,
             Lower_Middle2,
             Upper_Middle2,
             High2)

# WHO regions  
AFRO2 <- rgb(202, 122, 245, maxColorValue = max_out)
AMRO2 <- rgb(252, 219, 163, maxColorValue = max_out)
EMRO2 <- rgb(152, 230, 0, maxColorValue = max_out)
EURO2 <- rgb(158, 187, 215, maxColorValue = max_out)
SEARO2 <- rgb(196, 219, 212, maxColorValue = max_out)
WPRO2 <- rgb(252, 204, 182, maxColorValue = max_out)

who_col2 <- c(AFRO2,
              AMRO2,
              EMRO2,
              EURO2,
              SEARO2,
              WPRO2)

# get alternating colour vectors
who_col_alt <- c(rbind(who_col, who_col2))
wb_col_alt <- c(rbind(wb_col, wb_col2))

# WHO regions

tiff('shared/figs/WHO_region_bars_600dpi.tif',
     width = 10.5,
     height = 7.425,
     units = 'cm',
     res = 600,
     pointsize = 9,
     compression = 'lzw')

par(mar = c(5, 5, 3, 1))

coords <- barplot(as.matrix(t(who_regions[, c('X2005', 'X2012')])),
                  beside = TRUE,
                  border = NA,
                  names.arg = rep('', nrow(who_regions)),
                  col = who_col_alt,
                  space = c(0.1, 1),
                  xlab = '',
                  ylab = 'Incoming passenger volume (millions)',
                  ylim = c(0, 7.5 * 10 ^ 6),
                  axes = FALSE)

pop_seq <- c(0, 2, 4, 6)
pop_seq_labs <- c('', as.character(pop_seq[-1]))

axis(side = 2,
     at = pop_seq * 10 ^ 6,
     labels = pop_seq_labs,
     lwd.ticks = 0,
     las = 1,
     col = grey(0.4))

axis(side = 1,
     at = coords[1, ] + diff(coords[, 1]) / 2 ,
     labels = who_regions$region,
     tick = FALSE,
     las = 2,
     col = grey(0.4))

change_labs <- round(who_regions$X2012 / who_regions$X2005, 2)

text(x = coords[1, ] + diff(coords[1:2, 1] / 2),
     adj = 0.5,
     y = pmax(who_regions$X2005, who_regions$X2012) + 0.4 * 10 ^ 6,
     labels = change_labs,
     col = grey(0.2),
     cex = 1)



dev.off()



# World Bank regions

tiff('shared/figs/WB_region_bars_600dpi.tif',
     width = 10.5,
     height = 7.425,
     units = 'cm',
     res = 600,
     pointsize = 9,
     compression = 'lzw')

par(mar = c(5, 5, 3, 1))

# flip world bank regions
wb_regions <- wb_regions[nrow(wb_regions):1, ]

coords <- barplot(as.matrix(t(wb_regions[, c('X2005', 'X2012')])),
                  beside = TRUE,
                  border = NA,
                  names.arg = rep('', nrow(wb_regions)),
                  col = wb_col_alt,
                  space = c(0.1, 1),
                  xlab = '',
                  ylab = 'Incoming passenger volume (millions)',
                  ylim = c(0, 7.5 * 10 ^ 6),
                  axes = FALSE)

pop_seq <- c(0, 2, 4, 6)
pop_seq_labs <- c('', as.character(pop_seq[-1]))

axis(side = 2,
     at = pop_seq * 10 ^ 6,
     labels = pop_seq_labs,
     lwd.ticks = 0,
     las = 1,
     col = grey(0.4))

axis(side = 1,
     at = coords[1, ] + diff(coords[, 1]) / 2 ,
     labels = wb_regions$region,
     tick = FALSE,
     las = 2,
     col = grey(0.4))

change_labs <- round(wb_regions$X2012 / wb_regions$X2005, 2)

text(x = coords[1, ] + diff(coords[1:2, 1] / 2),
     adj = 0.5,
     y = pmax(wb_regions$X2005, wb_regions$X2012) + 0.4 * 10 ^ 6,
     labels = change_labs,
     col = grey(0.2),
     cex = 1)


dev.off()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# get statistics of interest

# set up empty results dataframe
res <- data.frame(row.names = 'value')

# ~~~~~~~~~~~~~~~~
# get PAR for set1 and set2 countries

# PAR for set 1 countries
res$set1_PAR <- sum(dat$population_at_risk[dat$has_cases])

# and for set 2 countries
res$set2_PAR <- sum(dat$population_at_risk[!dat$has_cases])

# for all
res$total_PAR <- sum(dat$population_at_risk)

# Get top three countries in each set
res$set1_top_three <- paste(dat$country[dat$has_cases][1:3], collapse = ', ')
res$set2_top_three <- paste(dat$country[!dat$has_cases][1:3], collapse = ', ')

# ~~~~~~~~~~~~~~~~
# population change in each set

res$set1_pop_before <- sum(dat$pop_1976[dat$has_cases])
res$set1_pop_after <- sum(dat$pop_2013[dat$has_cases])

res$set2_pop_before <- sum(dat$pop_1976[!dat$has_cases])
res$set2_pop_after <- sum(dat$pop_2013[!dat$has_cases])

res$total_pop_before <- sum(dat$pop_1976)
res$total_pop_after <- sum(dat$pop_2013)

# change in urban proportion
res$set1_urb_before <- round(sum(dat$U1976[dat$has_cases]) /
                               sum(dat$R1976[dat$has_cases]),
                             3)
res$set1_urb_after <- round(sum(dat$U2013[dat$has_cases]) /
                              sum(dat$R2013[dat$has_cases]),
                            3)

res$set2_urb_before <- round(sum(dat$U1976[!dat$has_cases]) /
                               sum(dat$R1976[!dat$has_cases]),
                             3)
res$set2_urb_after <- round(sum(dat$U2013[!dat$has_cases]) /
                              sum(dat$R2013[!dat$has_cases]),
                            3)

res$total_urb_before <- round(sum(dat$U1976) /
                                sum(dat$R1976),
                              3)
res$total_urb_after <- round(sum(dat$U2013) /
                               sum(dat$R2013),
                             3)


# changes in passenger volumes

# set 1
res$set1_pass_vol_2005 <- sum(dat$passengers_2005[dat$has_cases])
res$set1_pass_vol_2012 <- sum(dat$passengers_2012[dat$has_cases])
res$set1_pass_vol_ratio <- res$set1_pass_vol_2012 / res$set1_pass_vol_2005

# set 2
res$set2_pass_vol_2005 <- sum(dat$passengers_2005[!dat$has_cases])
res$set2_pass_vol_2012 <- sum(dat$passengers_2012[!dat$has_cases])
res$set2_pass_vol_ratio <- res$set2_pass_vol_2012 / res$set2_pass_vol_2005

# all 
res$total_pass_vol_2005 <- sum(dat$passengers_2005)
res$total_pass_vol_2012 <- sum(dat$passengers_2012)
res$total_pass_vol_ratio <- res$total_pass_vol_2012 / res$total_pass_vol_2005


# changes in seat capacity

# set 1
res$set1_seat_cap_2005 <- sum(dat$capacity_2000[dat$has_cases])
res$set1_seat_cap_2012 <- sum(dat$capacity_2013[dat$has_cases])
res$set1_seat_cap_ratio <- res$set1_seat_cap_2012 / res$set1_seat_cap_2005

# set 2
res$set2_seat_cap_2005 <- sum(dat$capacity_2000[!dat$has_cases])
res$set2_seat_cap_2012 <- sum(dat$capacity_2013[!dat$has_cases])
res$set2_seat_cap_ratio <- res$set2_seat_cap_2012 / res$set2_seat_cap_2005

# all 
res$total_seat_cap_2005 <- sum(dat$capacity_2000)
res$total_seat_cap_2012 <- sum(dat$capacity_2013)
res$total_seat_cap_ratio <- res$total_seat_cap_2012 / res$total_seat_cap_2005

write.csv(res,
          file = 'output/results_numbers.csv')
