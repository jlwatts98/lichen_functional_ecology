# lidar data
# https://portal.opentopography.org/rasterOutput?jobId=rt1682530846759
# set working directory
setwd("~/R/lichen_spectra")
# tar.gz file
#untar("CHM.tar.gz")
#untar("rasters_BcCZO10Aug.tar.gz")

library(raster)

CHM = raster("CHM.tif")
plot(CHM)

DTM = raster("output.dtm.tif")
plot(DTM)

e = extent(470000, 480000, 4425000, 4435000)
e2 = extent(470000, 476500, 4425000, 4430000)
CHM_crop = crop(CHM, e)
plot(CHM_crop)

CHM_crop2 = crop(CHM, e2)
plot(CHM_crop2)
