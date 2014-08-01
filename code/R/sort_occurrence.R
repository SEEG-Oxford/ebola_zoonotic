# clean ebola occurrence data for modelling

# clear workspace
rm(list = ls())

# define functions

firstDay <- function (year, month) {
  # given a year and month, return a Date object of the first day of that month
  date_string <- paste(dat$Year.Start,
                       dat$Month.Start,
                       '01',
                       sep = '-')
  
  date <- as.Date (date_string)
  
  return (date)
  
}

lastDay <- function (year, month) {
  # given a year and month, return a Aate object of the last day of that month
  next_month <- ifelse(month == 12,
                       1,
                       month + 1)
  
  next_year <- ifelse(month == 12,
                      year + 1,
                      year)
  
  next_date_string <- paste(next_year,
                            next_month,
                            '01',
                            sep = '-')
  next_date <- as.Date(next_date_string)
  date <- next_date - 1
  
  return (date)
}

#~~~~~~~~~

# load data
dat <- read.csv('data/occurrence/raw/Ebola_distribution_AM.csv')

# convert start and end dates into individual columns
dat$start_date <- firstDay(dat$Year.Start,
                           dat$Month.Start)

dat$end_date <- lastDay(dat$Year.End,
                           dat$Month.End)

# keep only the ID, virus, lat, long and start/end dates
dat <- dat[, c('ID',
               'Virus',
               'Lat',
               'Long',
               'start_date',
               'end_date')]

names(dat) <- tolower(names(dat))

write.csv(dat, file = 'data/occurrence/clean/occurrence.csv',
          row.names = FALSE)