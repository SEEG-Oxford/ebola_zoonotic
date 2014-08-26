#! /bin/bash

# copy over predicted rasters to the MAP drive

cp ~/Dropbox/SEEG_mapping/ebola/output/bernoulli/EBOV.tif ~/Z/zhi/ebola/predictions/EBOV.tif

# country risk estimates

cp ~/Dropbox/SEEG_mapping/ebola/output/bernoulli/country_risk.csv ~/Z/zhi/ebola/predictions/country_risk.csv

# bat predictions

cp ~/Dropbox/SEEG_mapping/ebola/output/bats/Hypsignathus_monstrosus/Hypsignathus_monstrosus.tif ~/Z/zhi/ebola/predictions/

cp ~/Dropbox/SEEG_mapping/ebola/output/bats/Epomops_franqueti/Epomops_franqueti.tif ~/Z/zhi/ebola/predictions/

cp ~/Dropbox/SEEG_mapping/ebola/output/bats/Myonycteris_torquata/Myonycteris_torquata.tif ~/Z/zhi/ebola/predictions/


# combined bat layers

cp ~/Dropbox/SEEG_mapping/ebola/output/bats/primary_mean.tif ~/Z/zhi/ebola/predictions/primary_bats_mean.tif

