# script to run all analysis, in theory

# get eh covariate layers together
source('code/R/combine_5km_rasters.R')

# get bat data and run models
source('code/R/make_bat_rasters.R')

source('code/R/get_bat_occurrence.R')

source('code/R/sort_bat_occurrence.R')

source('code/R/bat_niche_model.R')

source('code/R/combine_bat_predictions.R')

# sort EBOV occurrence data and run models
source('code/R/sort_occurrence.R')

source('code/R/bernoulli_niche_model.R')

# summarize models and calculate statistics
source('code/R/summarize_models.R')

source('code/R/calc_PAR.R')

source('code/R/predicting_guinea.R')

# plotting population and flights data
source('code/R/population_flight_plots.R')