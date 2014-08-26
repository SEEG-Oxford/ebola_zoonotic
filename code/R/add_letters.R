# add letters to figures

# clear workspace
rm(list = ls())

library(raster)

cap <- brick('~/Dropbox/SEEG_mapping/ebola/shared/figs/Seat_capacity_600dpi.tif') 
pas <- brick('~/Dropbox/SEEG_mapping/ebola/shared/figs/Passengers_600dpi.tif') 


tiff('shared/figs/Figure 7.tif',
     width = 21,
     height = 17.4,
     units = 'cm',
     res = 600,
     pointsize = 9,
     compression = 'lzw')

par(mar = rep(0, 4),
    mfrow = c(2, 1))

plotRGB(cap,
        maxpixels = Inf)

mtext(text = 'A',
      adj = 0.05,
      cex = 1.5,
      line = -1.2)

plotRGB(pas,
        maxpixels = Inf)

mtext(text = 'B',
      adj = 0.05,
      cex = 1.5,
      line = -1.2)

dev.off()
      # a seat cap