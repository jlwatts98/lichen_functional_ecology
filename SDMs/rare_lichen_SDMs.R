##### Rare Lichen Species Distribution Models #####
# set working directory
setwd("~/R/lichen_spectra")


# resources
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-biodiversity-cmip5-global?tab=overview
##### Reading in Geology Layers #####
library(raster)
library(rgdal)
library(sf)
library(terra)
library(stars)

# https://swregap.org/data/geology/

CO_GEO = read_sf("CO_geology/swgeology/swgeology.shp")
CO_GEO_rast = st_rasterize(CO_GEO)
plot(CO_GEO_rast)

# https://mrdata.usgs.gov/geology/state/

library(sf)
library(fasterize)
US_geology = read_sf("CO_geology/USGS_Shapefiles/SGMC_Geology.shp")
class(US_geology)

# reproject
WGS = "+proj=longlat +datum=WGS84 +no_defs"

#geo_reproj = st_transform(US_geology,
                        # crs = WGS)

# convert shapefile to raster
rast = fasterize::raster(US_geology, res = 1000)

GEO_rast = fasterize(US_geology, rast, field = "RuleID",
                     fun = "first")

plot(GEO_rast)

class(GEO_rast)

clims[[1]]
geo_project = projectRaster(GEO_rast, 
                            res = 0.008928571, 
                            crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84",
                            method = 'ngb',
                            over = T)


#WGS = "+proj=longlat +datum=WGS84 +no_defs"
# reproject geology raster
#library(proj4)
#geo_reproj = projectRaster(GEO_rast, crs = WGS)
#geo_reproj = projectRaster(from = GEO_rast, to = clims_crop[[1]])
#geo_reproj = projectRaster(GEO_rast, crs = ('+proj=longlat'))

#plot(GEO_rast)

##### Reading in DEMs #####
library(terra)

DEM_files = list.files(path = "CO_DEM", pattern = "\\.tif$")
DEM_files = paste("CO_DEM/", DEM_files, sep = "")
DEM = sprc(DEM_files)
CO_DEM = merge(DEM)

plot(CO_DEM)

# DEM for the entire world, lower resolution
# https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO2/ETOPO2v2-2006/ETOPO2v2c/netCDF/
library(ncdf4)
DEM = raster("CO_DEM/world/world.nc")
plot(DEM)

##### Reading in Vegetation Type Data #####
# https://forobs.jrc.ec.europa.eu/glc2000/data

vegetation = raster("Vegetation/namerica/Bil/namerica_v2.bil")
plot(vegetation)
cellStats(vegetation, stat = "min")
cellStats(vegetation, stat = "max")
cellStats(vegetation, stat = "mean")


##### Reading in Climate Data #####

clim_files = list.files(path = "climate", pattern = "\\.tif$")
clim_files
clim = paste("climate/", clim_files, sep = "")
bios = stack(clim)

clim_files = list.files(path = "climate", pattern = "\\.nc$")
clim = paste("climate/", clim_files, sep = "")
clims = stack(clim)

envirem = list.files(path = "envirem", pattern = "\\.tif$")
envirem = paste("envirem/", envirem, sep = "")
envirem = stack(envirem)

plot(clims[[1]])
names(bios)

summer_precip = bios[[10]]
winter_precip = bios[[11]]
ex = raster::extent(-109.046666, -102.046666, 37, 41)

CO_summer_precip = crop(summer_precip, ex)
CO_winter_precip = crop(winter_precip, ex)
CO_vegetation = crop(vegetation, ex)

plot(summer_precip)
plot(CO_summer_precip)
plot(CO_winter_precip)
plot(CO_vegetation)

# ecoregions level 5

CO_veg_vec = as.vector(CO_vegetation)

mode_value <- as.numeric(names(sort(table(CO_veg_vec), decreasing = TRUE)[1]))
hist(CO_veg_vec)
# vegetation sucks

points(-106.777200, 40.630170)
points(-105.634592, 40.078678)
points(-107.45, 37.87)


##### Standardize and Stack Raster data #####

# crop rasters to the US
e = raster::extent(-125, -60, 25, 50)

clims_crop = raster::crop(clims, e)
bios_crop = raster::crop(bios, e)
veg_crop = raster::crop(vegetation, e)
DEM_crop = raster::crop(DEM, e)
envirem_crop = raster::crop(envirem, e)

plot(clims[[1]])
plot(clims_crop[[1]])
plot(bios_crop[[1]])
plot(DEM_crop)
#GEO_crop = raster(GEO_rast, e)

res(clims_crop)
res(bios_crop)
res(veg_crop)
res(DEM_crop)
res(envirem_crop)

# resample raster to match resolution
veg_crop_resamp = resample(veg_crop, clims_crop)
DEM_crop_resamp = resample(DEM_crop, clims_crop)

res(veg_crop_resamp)
res(DEM_crop_resamp)

extent(clims_crop)
extent(bios_crop)
extent(veg_crop_resamp)
extent(DEM_crop_resamp)
extent(envirem_crop)

names(clims_crop)
names(bios_crop)

plot(clims_crop[[44]])

all = stack(clims_crop, bios_crop, veg_crop_resamp, DEM_crop_resamp, envirem_crop)

##### Assess autocorrelation and choose relevant variables #####

##### Omphalora Arizonica #####
##### Read in Occurrence Data #####
omphalora_arizonica = readr::read_csv("occurrences/Omphalora_arizonica.csv") |>
    dplyr::filter(!is.na(decimalLatitude)) |>
    dplyr::rename(lat = decimalLatitude,
                  lon = decimalLongitude)

omph_xy = omphalora_arizonica[, c(23,22)]

omph_sp <- SpatialPointsDataFrame(coords = omph_xy, data = omphalora_arizonica,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

ompha_env = raster::extract(all, omph_sp)

ompha_env = as.data.frame(ompha_env)

# explore
library(ggplot2)

names(ompha_env)

ggplot(ompha_env, aes(x = vapor_pressure_deficit.8)) +
    geom_histogram() +
    theme_minimal()

ggplot(ompha_env, aes(x = current_2.5arcmin_climaticMoistureIndex)) +
    geom_histogram() +
    theme_minimal()

ggplot(ompha_env, aes(x = namerica_v2)) +
    geom_histogram() +
    theme_minimal()

ggplot(ompha_env, aes(x = z)) +
    geom_histogram() +
    theme_minimal()

cor_mat = cor(ompha_env)
cor_mat

# reduce raster stack
names(ompha_env)

# 20 august downward_shortwave_flux,
# 26 february snow water equivalent
# 44 august vpd
# 49 annual mean temp
# 50 summer mean temp
# 52 annual precipitation
# 58 summer precip
# 68 climatic moisture index
# 69 vegetation type (factor!)
# 70 elevation
# 71 annual Potential Evaptranspiration
# 72 aridity Index

all_red = all[[c(20, 26, 44, 49, 50, 52, 58, 68, 70, 71, 72)]]

all_red_crop = crop(all_red, ex)


ompha_env_red = raster::extract(all_red, omph_sp)

ompha_env_red = as.data.frame(ompha_env_red)

cor_mat_red = cor(ompha_env_red)
cor_mat_red

# option to reduce more!

##### Run SDMs #####
library(sdm)
library(SDMtune)
library(checkmate)
library(rgdal)
library(dismo)

# define a function that get's pseudoabsence points
get_backgr = function(
        presence,
        samp_dist,
        samp_num,
        predictors
){
    
    #checks
    checkmate::assert_data_frame(presence)
    checkmate::assert_numeric(samp_dist)
    checkmate::assert_numeric(samp_num)
    
    # selecting background points
    # make into spatial
    coordinates(presence) = ~ lon + lat
    projection(presence) = CRS('+proj=longlat +datum=WGS84')
    
    # circles with a radius of d
    x = circles(presence, d = samp_dist, lonlat = TRUE)
    pol = polygons(x)
    
    # sample randomly from all circles
    samp1 = spsample(pol, samp_num, type = 'random', iter = 25)
    
    # make into dataframe
    background = as.data.frame(samp1)
    
    # return the result
    return(background)
    
}


# get background points
backgr = get_backgr(omphalora_arizonica[c(23,22)], 2000000,
                          1000, all_red[[1]])

# doesn't use raster anymore! terra package.
# backgr = SDMtune::thinData(backgr, all_red[[1]])


backgr = backgr |>
    dplyr::rename(lon = x,
                  lat = y)

ompha_df = rbind(omph_xy, backgr)
ompha_df$oc = c(rep(1, nrow(omph_xy)), rep(0, nrow(backgr)))

coordinates(ompha_df) = ~ lon + lat

d_ompha <- sdmData(formula=oc~., 
                   train=ompha_df, 
                   predictors = all_red)

# fit the models (7 methods, and 10 replications using bootstrapping procedure):
m_ompha <- sdm(oc~.,
               data = d_ompha, 
               methods=c('rf', 'gbm', 'gam'),
               replicatin='boot',
               n=10)


predict_ompha = ensemble(m_ompha, newdata=all_red_crop, filename='ompha_test.img',
                       overwrite = T,
                       setting=list(method='weighted',
                                    stat='AUC'))

writeRaster(predict_ompha, filename = "ompha_output.tif", overwrite=TRUE)
plot(predict_ompha)
points(omph_xy)



#sw = extent(-120, -102.046666, 31.3333, 42)
#predict_SW = crop(predict, sw)

jpeg(file="omphalora_arizonica_CO.jpeg", height = 5,
     width = 6, units = "in",
     res = 600)
plot(predict_CO)
dev.off()

#jpeg(file="omphalora_arizonica.jpeg", height = 5,
#     width = 7, units = "in",
#     res = 600)
#plot(predict_SW)
#dev.off()


##### Circinaria rogeri #####
##### Read in Occurrence Data #####
circinaria_rogeri = readr::read_csv("occurrences/circinaria_rogeri.csv") |>
    dplyr::filter(!is.na(decimalLatitude)) |>
    dplyr::rename(lat = decimalLatitude,
                  lon = decimalLongitude)

names(circinaria_rogeri)

circ_xy = circinaria_rogeri[, c(75,74)]

circ_sp <- SpatialPointsDataFrame(coords = circ_xy, data = circinaria_rogeri,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

plot(clims_crop[[1]])
points(circ_sp)

library(raster)
circi_env = raster::extract(all, circ_sp)

circi_env = as.data.frame(circi_env)

# explore
library(ggplot2)

names(circi_env)

ggplot(circi_env, aes(x = vapor_pressure_deficit.8)) +
    geom_histogram() +
    theme_minimal()

ggplot(circi_env, aes(x = current_2.5arcmin_climaticMoistureIndex)) +
    geom_histogram() +
    theme_minimal()

ggplot(circi_env, aes(x = namerica_v2)) +
    geom_histogram() +
    theme_minimal()

ggplot(circi_env, aes(x = z)) +
    geom_histogram() +
    theme_minimal()

cor_mat = cor(circi_env)
cor_mat

# reduce raster stack
names(circi_env)

#all_red = all[[c(20, 26, 44, 49, 50, 52, 58, 68, 70, 71, 72)]]

circi_env_red = raster::extract(all_red, circ_sp)

circi_env_red = as.data.frame(circi_env_red)

circi_env_red$lon = circ_sp$lon
circi_env_red$lat = circ_sp$lat

cor_mat_red = cor(circi_env_red)
cor_mat_red

circi_env_red = na.omit(circi_env_red)



# option to reduce more!

##### Run SDMs #####

# get background points
backgr = get_backgr(circi_env_red[c(12,13)], 2000000,
                    1000, all_red[[1]])

# doesn't use raster anymore! terra package.
# backgr = SDMtune::thinData(backgr, all_red[[1]])


backgr = backgr |>
    dplyr::rename(lon = x,
                  lat = y)

circi_df = rbind(circi_env_red[,c(12,13)], backgr)
circi_df$oc = c(rep(1, nrow(circi_env_red)), rep(0, nrow(backgr)))

coordinates(circi_df) = ~ lon + lat

d_circi <- sdmData(formula=oc~., 
                   train=circi_df, 
                   predictors = all_red)

# fit the models (7 methods, and 10 replications using bootstrapping procedure):
m_circi <- sdm(oc~.,
               data = d_circi, 
               methods=c('rf', 'gbm', 'gam'),
               replicatin='boot',
               n=10)

plot(all_red_crop[[1]])

# predict for only colorado to save time
predict_circi = ensemble(m_circi, newdata=all_red_crop, filename='circi_test.img',
                   overwrite = T,
                   setting=list(method='weighted',
                                stat='AUC'))

writeRaster(predict_circi, filename = "circi_output.tif")
plot(predict_circi)
points(circ_xy)



jpeg(file="circinaria_rogeri_CO.jpeg", height = 5,
     width = 6, units = "in",
     res = 600)
plot(predict_circi)
dev.off()


##### Leprocaulon americanum #####
##### Read in Occurrence Data #####
leprocaulon_americanum = readr::read_csv("occurrences/Leprocaulon_americanum.csv") |>
    dplyr::filter(!is.na(decimalLatitude)) |>
    dplyr::rename(lat = decimalLatitude,
                  lon = decimalLongitude)

names(leprocaulon_americanum)

lepro_xy = leprocaulon_americanum[, c(69,68)]

lepro_sp <- SpatialPointsDataFrame(coords = lepro_xy, data = leprocaulon_americanum,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

plot(clims_crop[[1]])
points(lepro_sp)

library(raster)
lepro_env = raster::extract(all, lepro_sp)

lepro_env = as.data.frame(lepro_env)

# explore
library(ggplot2)

names(lepro_env)

ggplot(lepro_env, aes(x = vapor_pressure_deficit.8)) +
    geom_histogram() +
    theme_minimal()

ggplot(lepro_env, aes(x = current_2.5arcmin_climaticMoistureIndex)) +
    geom_histogram() +
    theme_minimal()

ggplot(lepro_env, aes(x = namerica_v2)) +
    geom_histogram() +
    theme_minimal()

ggplot(lepro_env, aes(x = z)) +
    geom_histogram() +
    theme_minimal()

cor_mat = cor(lepro_env)
cor_mat

# reduce raster stack
names(lepro_env)

#all_red = all[[c(20, 26, 44, 49, 50, 52, 58, 68, 70, 71, 72)]]

lepro_env_red = raster::extract(all_red, lepro_sp)

lepro_env_red = as.data.frame(lepro_env_red)

lepro_env_red$lon = lepro_sp$lon
lepro_env_red$lat = lepro_sp$lat

cor_mat_red = cor(lepro_env_red)
cor_mat_red

lepro_env_red = na.omit(lepro_env_red)



# option to reduce more!

##### Run SDMs #####

# get background points
backgr = get_backgr(lepro_env_red[c(12,13)], 2000000,
                    1000, all_red[[1]])

# doesn't use raster anymore! terra package.
# backgr = SDMtune::thinData(backgr, all_red[[1]])


backgr = backgr |>
    dplyr::rename(lon = x,
                  lat = y)

lepro_df = rbind(lepro_env_red[,c(12,13)], backgr)
lepro_df$oc = c(rep(1, nrow(lepro_env_red)), rep(0, nrow(backgr)))

coordinates(lepro_df) = ~ lon + lat

d_lepro <- sdmData(formula=oc~., 
                   train=lepro_df, 
                   predictors = all_red)

# fit the models (7 methods, and 10 replications using bootstrapping procedure):
m_lepro <- sdm(oc~.,
               data = d_lepro, 
               methods=c('rf', 'gbm', 'gam'),
               replicatin='boot',
               n=10)

plot(all_red_crop[[1]])

# predict for only colorado to save time
predict_lepro = ensemble(m_lepro, newdata=all_red_crop, filename='lepro_test.img',
                         overwrite = T,
                         setting=list(method='weighted',
                                      stat='AUC'))

writeRaster(predict_lepro, filename = "lepro_output.tif")
plot(predict_lepro)
points(lepro_xy)



jpeg(file="leprocaulon_americanum_CO.jpeg", height = 5,
     width = 6, units = "in",
     res = 600)
plot(predict_lepro)
dev.off()

##### Myriolecis wetmorei #####
##### Read in Occurrence Data #####
myriolecis_wetmorei = readr::read_csv("occurrences/Myriolecis_wetmorei.csv") |>
    dplyr::filter(!is.na(decimalLatitude)) |>
    dplyr::rename(lat = decimalLatitude,
                  lon = decimalLongitude)

names(myriolecis_wetmorei)

myrio_xy = myriolecis_wetmorei[, c(69,68)]

myrio_sp <- SpatialPointsDataFrame(coords = myrio_xy, data = myriolecis_wetmorei,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

plot(clims_crop[[1]])
points(myrio_sp)

library(raster)
myrio_env = raster::extract(all, myrio_sp)

myrio_env = as.data.frame(myrio_env)

# explore
library(ggplot2)

names(myrio_env)

ggplot(myrio_env, aes(x = vapor_pressure_deficit.8)) +
    geom_histogram() +
    theme_minimal()

ggplot(myrio_env, aes(x = current_2.5arcmin_climaticMoistureIndex)) +
    geom_histogram() +
    theme_minimal()

ggplot(myrio_env, aes(x = namerica_v2)) +
    geom_histogram() +
    theme_minimal()

ggplot(myrio_env, aes(x = z)) +
    geom_histogram() +
    theme_minimal()

cor_mat = cor(myrio_env)
cor_mat

# reduce raster stack
names(myrio_env)

#all_red = all[[c(20, 26, 44, 49, 50, 52, 58, 68, 70, 71, 72)]]

myrio_env_red = raster::extract(all_red, myrio_sp)

myrio_env_red = as.data.frame(myrio_env_red)

myrio_env_red$lon = myrio_sp$lon
myrio_env_red$lat = myrio_sp$lat

cor_mat_red = cor(myrio_env_red)
cor_mat_red

myrio_env_red = na.omit(myrio_env_red)



# option to reduce more!

##### Run SDMs #####

# get background points
backgr = get_backgr(myrio_env_red[c(12,13)], 2000000,
                    1000, all_red[[1]])

# doesn't use raster anymore! terra package.
# backgr = SDMtune::thinData(backgr, all_red[[1]])


backgr = backgr |>
    dplyr::rename(lon = x,
                  lat = y)

myrio_df = rbind(myrio_env_red[,c(12,13)], backgr)
myrio_df$oc = c(rep(1, nrow(myrio_env_red)), rep(0, nrow(backgr)))

coordinates(myrio_df) = ~ lon + lat

d_myrio <- sdmData(formula=oc~., 
                   train=myrio_df, 
                   predictors = all_red)

# fit the models (7 methods, and 10 replications using bootstrapping procedure):
m_myrio <- sdm(oc~.,
               data = d_myrio, 
               methods=c('rf', 'gbm', 'gam'),
               replicatin='boot',
               n=10)

plot(all_red_crop[[1]])

# predict for only colorado to save time
predict_myrio = ensemble(m_myrio, newdata=all_red_crop, filename='myrio_test.img',
                         overwrite = T,
                         setting=list(method='weighted',
                                      stat='AUC'))

writeRaster(predict_myrio, filename = "myrio_output.tif")
plot(predict_myrio)
points(myrio_xy)



jpeg(file="myriolecis_wetmorei_CO.jpeg", height = 5,
     width = 6, units = "in",
     res = 600)
plot(predict_myrio)
dev.off()

##### Plot #####

# load in old prediction for omphalora
predict_ompha = raster("omphalora_output.tif")
predict_ompha = crop(predict_ompha, ex)
plot(predict_ompha)
plot(predict_circi)
plot(predict_lepro)
plot(predict_myrio)

predict_stack = stack(predict_ompha, predict_circi,
                      predict_lepro, predict_myrio)

rasternames = c("Omphalora arizonica",
                "Circinaria rogeri",
                "Leprocaulon americanum",
                "Myriolecis wetmorei")

library(rasterVis)
p.strip <- list(cex=1.2, lines=1, col="black", fontface='italic')


# use level plot to create a nice plot with one legend and a 4x4 layout.
final_plot = rasterVis::levelplot(predict_stack,
          layout=c(2, 2), 
          #col.regions=cols, # add a color ramp
          main="Rare Colorado Lichen SDMs",
          names.attr=rasternames,
          par.strip.text = p.strip,
          scales=list(draw=FALSE ),
          xlab = "",
          ylab = "",
          colorkey = list(title = list("POC",
                                       cex = 1.5,
                                       #fontface = "bold",
                                       col = "black")
          ))

final_plot

# save
jpeg(filename = "prelim_SDMs.jpeg", width = 7, height = 5,
     units = "in",
     res = 600)
rasterVis::levelplot(predict_stack,
                     layout=c(2, 2), 
                     #col.regions=cols, # add a color ramp
                     main="Rare Colorado Lichen SDMs",
                     names.attr=rasternames,
                     par.strip.text = p.strip,
                     scales=list(draw=FALSE ),
                     xlab = "",
                     ylab = "",
                     colorkey = list(title = list("POC",
                                                  cex = 1.5,
                                                  #fontface = "bold",
                                                  col = "black")
                     ))
dev.off()
