# extract statistics for the EBOV and bat models to quote in the text

# clear workspace
rm(list = ls())

# source functions files
source('code/R/functions.R')

paths <- c('output/bats/Epomops_franqueti/',
           'output/bats/Hypsignathus_monstrosus/',
           'output/bats/Myonycteris_torquata/',
           'output/bernoulli/')

names <- c('Epomops franqueti',
           'Hypsignathus monstrosus',
           'Myonycteris torquata',
           'EBOV')

sry <- lapply(paths, summarizeStats)

sry <- do.call(cbind, sry)
sry <- data.frame(sry)
names(sry) <- names

print(sry)

write.csv(sry,
          file = 'output/model_summary.csv')
