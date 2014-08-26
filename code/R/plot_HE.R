# plot health care expenditrue for set 1 countries against regional summaries

# clear workspace
rm(list = ls())

# load data

# PAR estimates by country (contians Set 1 countries)
PAR <- read.csv('output/bernoulli/country_risk.csv',
                stringsAsFactors = FALSE)

# Health care expenditure by country
HE <- read.csv('data/population_flights/health_expenditure.csv',
               stringsAsFactors = FALSE)

# under5 mortality by country
U5 <- read.csv('data/population_flights/u5_mortality.csv',
               stringsAsFactors = FALSE)

# country WHO regional office affiliations
WHO <- read.csv('data/population_flights/who_countries.csv',
                stringsAsFactors = FALSE)

# shoten names in PAR
PAR$country[PAR$country == 'Central African Republic'] <- 'CAR'
PAR$country[PAR$country == 'Democratic Republic of the Congo'] <- 'DRC'
PAR$country[PAR$country == 'Equatorial Guinea'] <- 'Eq. Guinea'
PAR$country[PAR$country == 'Congo'] <- 'ROC'
PAR$country[PAR$country == 'United Republic of Tanzania'] <- 'Tanzania'

# ~~~~~~~~~
# lining up data

# match up U5 data to HE data

# rename these to match HE
U5$country[U5$country == "Cabo Verde"] <- "Cabo Verde Republic of"
U5$country[U5$country == "Bolivia (Plurinational State of)"] <- "Bolivia Plurinational States of "

# remove these countries not in HE
U5 <- U5[!(U5$country %in% c("Democratic People's Republic of Korea",
                             "Somalia",
                             "Sri Lanka",
                             "Zimbabwe")), ]

# match country names with HE
idx <- match(HE$Countries, U5$country)
U5 <- U5[idx, ]

# add U5 data to HE
HE$u5 <- U5$u5_mort_1000_2012

# change HE names to WHO region names
HE$Countries[HE$Countries == "Cote d'Ivoire"] <- "Côte d'Ivoire"
HE$Countries[HE$Countries == "Cabo Verde Republic of"  ] <- "Cape Verde"
HE$Countries[HE$Countries == "Gambia"] <- "The Gambia"
HE$Countries[HE$Countries == "United Republic of Tanzania"] <- "Tanzania (United Republic of)"
HE$Countries[HE$Countries == "Bolivia Plurinational States of "] <- "Bolivia"
HE$Countries[HE$Countries == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
HE$Countries[HE$Countries == "Iran (Islamic Republic of)"] <- "Iran"
HE$Countries[HE$Countries == "Libya"] <- "Libyan Arab Jamahiriya"
HE$Countries[HE$Countries == "Republic of Moldova"] <- "Moldova, Republic of"
HE$Countries[HE$Countries == "The former Yugoslav Republic of Macedonia"] <- "Macedonia, the former Yugoslav Republic of"
HE$Countries[HE$Countries == "Micronesia (Federated States of)"] <- "Micronesia, Federated States of"
HE$Countries[HE$Countries == "Republic of Korea"] <- "Korea, Republic of"
HE$Countries[HE$Countries == "Viet Nam"] <- "Vietnam"

# remove Sao Tome and Principe (not in WHO regions data)
HE <- HE[HE$Countries != "Sao Tome and Principe", ]

# ~~~~~~~~~~
# combining data

# keep only WHO countries represented in HE data
WHO <- WHO[WHO$country %in% HE$Countries, ]

# re-order WHO to same as HE
idx <- match(HE$Countries, WHO$country)
WHO <- WHO[idx, ]

# append to HE data
HE$region <- WHO$region

# now change Tanzania to Tanzania to match PAR and other plots
HE$Countries[HE$Countries == 'Central African Republic'] <- 'CAR'
HE$Countries[HE$Countries == 'Democratic Republic of the Congo'] <- 'DRC'
HE$Countries[HE$Countries == 'Equatorial Guinea'] <- 'Eq. Guinea'
HE$Countries[HE$Countries == 'Congo'] <- 'ROC'
HE$Countries[HE$Countries == 'United Republic of Tanzania'] <- 'Tanzania'
HE$Countries[HE$Countries == "Tanzania (United Republic of)"] <- 'Tanzania'

# calculate regional  and national summaries

# define sets
regions <- unique(HE$region)

# get regional stats
stats_regions <- sapply(regions,
                        function (region, HE) {
                          idx <- HE$region == region
                          c(HE = median(HE$PPP_HCE_PC_2012[idx]),
                            U5 = median(HE$u5[idx]))
                        },
                        HE)

# get at-risk country stats
stats_countries <- sapply(PAR$country,
                          function (country, HE) {
                            idx <- HE$Countries == country
                            c(HE = median(HE$PPP_HCE_PC_2012[idx]),
                              U5 = median(HE$u5[idx]))
                          },
                          HE)

# value for the AFRO region
AFRO_value <- stats_regions[,
                            colnames(stats_regions) == 'AFRO']

# ~~~~~~~~~
# set up plotting values

# ~~~~~~
# colours

# WHO regions  
AFRO <- rgb(202, 122, 245, maxColorValue = 255)
AMRO <- rgb(252, 219, 163, maxColorValue = 255)
EMRO <- rgb(152, 230, 0, maxColorValue = 255)
EURO <- rgb(158, 187, 215, maxColorValue = 255)
SEARO <- rgb(196, 219, 212, maxColorValue = 255)
WPRO <- rgb(252, 204, 182, maxColorValue = 255)

# red in map
map_red <- rgb(214, 47, 39, maxColorValue = 255)

# define vector of colours
colvec <- c(AFRO,
            AMRO,
            EMRO,
            EURO,
            SEARO,
            WPRO,
            rep(map_red,
                ncol(stats_countries)))


# ~~~~~~~~~~
# healthcare expenditure per capita

stat_idx <- 1  # healthcare expenditure
# stat_idx <- 2  # under-5 mortality

names <- c(colnames(stats_regions),
           colnames(stats_countries))

stats <- c(stats_regions[stat_idx, ],
           stats_countries[stat_idx, ])

tiff('shared/figs/healthcare_expenditure.tif',
     width = 21,
     height = 8.7,
     units = 'cm',
     res = 600,
     pointsize = 9,
     compression = 'lzw')

par(mar = c(7, 7, 2, 1))

coords <- barplot(stats,
                  names.arg = names,
                  border = NA,
                  col = colvec,
                  ylim = c(0, 2000),
                  las = 2,
                  axes = FALSE)

title(ylab = 'Healthcare expenditure per capita',
      line = 5,
      cex.lab = 1.2)

axis(side = 2,
     at = c(0,
            AFRO_value[stat_idx],
            1000,
            2000),
     las = 1,
     labels = c('',
                paste0('$',
                       round(AFRO_value[stat_idx])),
                '$1,000',
                '$2,000'),
     lwd.tick = 0)

set1_x <- range(coords[6 + (1:7), 1]) + c(-0.5, 0.5) * diff(coords[1:2, 1])

# plot a rectangle
rect(set1_x[1],
     -10,
     set1_x[2],
     2000,
     border = NA,
     col = grey(0.9),
     xpd = NA)

lines(y = rep(AFRO_value[stat_idx], 2),
      x = c(0, max(coords) + 1),
      col = grey(0.4),
      lty = 2,
      lwd = 1)

barplot(stats,
        names.arg = NA,
        col = colvec,
        border = NA,
        add = TRUE,
        axes = FALSE)

dev.off()

# ~~~~~~~~~~
# under 5 mortality per 1000 live births

# stat_idx <- 1  # healthcare expenditure
stat_idx <- 2  # under-5 mortality

names <- c(colnames(stats_regions),
           colnames(stats_countries))

stats <- c(stats_regions[stat_idx, ],
           stats_countries[stat_idx, ])

tiff('shared/figs/u5_mortality.tif',
     width = 21,
     height = 8.7,
     units = 'cm',
     res = 600,
     pointsize = 9,
     compression = 'lzw')

par(mar = c(7, 7, 2, 1))

coords <- barplot(stats,
                  names.arg = names,
                  border = NA,
                  col = colvec,
                  ylim = c(0, 200),
                  las = 2,
                  axes = FALSE)

title(ylab = 'Under 5 mortality rate (per 1,000)',
      line = 5,
      cex.lab = 1.2)

axis(side = 2,
     at = c(0,
            AFRO_value[stat_idx],
            200),
     las = 1,
     labels = c('',
                round(AFRO_value[stat_idx]),
                '200'),
     lwd.tick = 0)

set1_x <- range(coords[6 + (1:7), 1]) + c(-0.5, 0.5) * diff(coords[1:2, 1])

# plot a rectangle
rect(set1_x[1],
     -2,
     set1_x[2],
     200,
     border = NA,
     col = grey(0.9),
     xpd = NA)

lines(y = rep(AFRO_value[stat_idx], 2),
      x = c(0, max(coords) + 1),
      col = grey(0.4),
      lty = 2,
      lwd = 1)

barplot(stats,
        names.arg = NA,
        col = colvec,
        border = NA,
        add = TRUE,
        axes = FALSE)

dev.off()
