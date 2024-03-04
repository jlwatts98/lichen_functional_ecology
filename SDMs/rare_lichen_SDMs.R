##### Rare Lichen Species Distribution Models #####

# source header
source("header.R")
# resources
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-biodiversity-cmip5-global?tab=overview

##### Reading in DEMs #####
library(terra)

DEM_files = list.files(path = "lfe_data/SDMs/CO_DEM", pattern = "\\.tif$")
DEM_files = paste("lfe_data/SDMs/CO_DEM/", DEM_files, sep = "")
DEM = sprc(DEM_files)
CO_DEM = merge(DEM)

plot(CO_DEM)

# DEM for the entire world, lower resolution
# https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO2/ETOPO2v2-2006/ETOPO2v2c/netCDF/

DEM = raster("lfe_data/SDMs/CO_DEM/world/world.nc")
plot(DEM)

##### Reading in Vegetation Type Data #####
# https://forobs.jrc.ec.europa.eu/glc2000/data

vegetation = raster("lfe_data/SDMs/Vegetation/namerica/Bil/namerica_v2.bil")
plot(vegetation)
cellStats(vegetation, stat = "min")
cellStats(vegetation, stat = "max")
cellStats(vegetation, stat = "mean")


##### Reading in Climate Data #####

clim_files = list.files(path = "lfe_data/SDMs/climate", pattern = "\\.tif$")
clim_files
clim = paste("lfe_data/SDMs/climate/", clim_files, sep = "")
bios = stack(clim)

clim_files = list.files(path = "lfe_data/SDMs/climate", pattern = "\\.nc$")
clim = paste("lfe_data/SDMs/climate/", clim_files, sep = "")
clims = stack(clim)

envirem = list.files(path = "lfe_data/SDMs/envirem", pattern = "\\.tif$")
envirem = paste("lfe_data/SDMs/envirem/", envirem, sep = "")
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


##### Reading in Geology Layers #####


# https://swregap.org/data/geology/

CO_GEO = read_sf("lfe_data/SDMs/CO_geology/swgeology/swgeology.shp")
CO_GEO_rast = st_rasterize(CO_GEO)
plot(CO_GEO_rast)

# https://mrdata.usgs.gov/geology/state/

US_geology = read_sf("lfe_data/SDMs/CO_geology/USGS_Shapefiles/SGMC_Geology.shp")
class(US_geology)
unique(US_geology$GENERALIZE)

# reproject
#WGS = "+proj=longlat +datum=WGS84 +no_defs"

#geo_reproj = st_transform(US_geology,
                        # crs = WGS)

# convert shapefile to raster
#rast = fasterize(US_geology, raster = )


#GEO_rast = fasterize(US_geology, rast, field = "RuleID",
#                     fun = "first")

#plot(GEO_rast)

#class(GEO_rast)

#clims[[1]]
#geo_project = projectRaster(GEO_rast, 
#                            res = 0.008928571, 
#                            crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84",
#                            method = 'ngb',
#                            over = T)


#WGS = "+proj=longlat +datum=WGS84 +no_defs"
# reproject geology raster
#library(proj4)
#geo_reproj = projectRaster(GEO_rast, crs = WGS)
#geo_reproj = projectRaster(from = GEO_rast, to = clims_crop[[1]])
#geo_reproj = projectRaster(GEO_rast, crs = ('+proj=longlat'))

#plot(GEO_rast)


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

##### National Forest Boundaries #####
NF <- st_read(dsn = "lfe_data/SDMs/CO_NF/colorado-national-forest.shp")
crs(NF)
crs(all_red_crop[[1]])
NF_reproj = st_transform(NF, crs = crs(all_red_crop[[1]]))
crs(NF_reproj)
NF_sp = sf::as_Spatial(NF_reproj$geometry)

plot(NF_sp)
# Convert colorado raster to points
raster_points <- as.data.frame(rasterToPoints(all_red_crop[[1]]))
stars_raster = st_as_stars(all_red_crop[[1]])


# Convert points to sf object
sf_points <- st_as_sf(raster_points, coords = c("x", "y"))

# Set the projection
st_crs(sf_points) <- st_crs(all_red_crop[[1]])

# Convert points to raster cells
sf_raster <- st_rasterize(sf_points, stars_raster)

# Plot the resulting sf object
plot(sf_raster)

NF_cropped = st_intersection(shapefile, sf_raster)
sf_use_s2(FALSE)
crop_extent <- st_bbox(c(xmin = 37, xmax = 41, ymin = -109.046666, ymax = -102.046666), crs = st_crs(NF))
NF_cropped = st_crop(NF, crop_extent)
plot(NF_cropped)

##### Omphalora Arizonica #####
### Read in Occurrence Data ###
omphalora_arizonica = readr::read_csv("lfe_data/SDMs/occurrences/Omphalora_arizonica.csv") |>
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


predict_ompha = ensemble(m_ompha, newdata=all_red_crop, filename='lfe_objects/SDMs/ompha_test.img',
                       overwrite = T,
                       setting=list(method='weighted',
                                    stat='AUC'))

writeRaster(predict_ompha, filename = "lfe_objects/SDMs/ompha_output.tif", overwrite=TRUE)
plot(predict_ompha)
points(omph_xy)



#sw = extent(-120, -102.046666, 31.3333, 42)
#predict_SW = crop(predict, sw)

jpeg(file="lfe_objects/SDMs/omphalora_arizonica_CO.jpeg", height = 5,
     width = 6, units = "in",
     res = 600)
plot(predict_ompha)
dev.off()

levelplot(predict_ompha)


##### Circinaria rogeri #####
### Read in Occurrence Data ###
circinaria_rogeri = readr::read_csv("lfe_data/SDMs/occurrences/circinaria_rogeri.csv") |>
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



# predict for only colorado to save time
predict_circi = ensemble(m_circi, newdata=all_red_crop, filename='lfe_objects/SDMs/circi_test.img',
                   overwrite = T,
                   setting=list(method='weighted',
                                stat='AUC'))

writeRaster(predict_circi, filename = "lfe_objects/SDMs/circi_output.tif",
            overwrite = T)
plot(predict_circi)
points(circ_xy)



jpeg(file="lfe_objects/SDMs/circinaria_rogeri_CO.jpeg", height = 5,
     width = 6, units = "in",
     res = 600)
plot(predict_circi)
dev.off()


##### Leprocaulon americanum #####
### Read in Occurrence Data ##
leprocaulon_americanum = readr::read_csv("lfe_data/SDMs/occurrences/Leprocaulon_americanum.csv") |>
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


# predict for only colorado to save time
predict_lepro = ensemble(m_lepro, newdata=all_red_crop, filename='lfe_objects/SDMs/lepro_test.img',
                         overwrite = T,
                         setting=list(method='weighted',
                                      stat='AUC'))

writeRaster(predict_lepro, filename = "lfe_objects/SDMs/lepro_output.tif",
            overwrite = T)
plot(predict_lepro)
points(lepro_xy)

jpeg(file="lfe_objects/SDMs/leprocaulon_americanum_CO.jpeg", height = 5,
     width = 6, units = "in",
     res = 600)
plot(predict_lepro)
dev.off()

##### Myriolecis wetmorei #####
## Read in Occurrence Data ##
myriolecis_wetmorei = readr::read_csv("lfe_data/SDMs/occurrences/Myriolecis_wetmorei.csv") |>
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


# predict for only colorado to save time
predict_myrio = ensemble(m_myrio, newdata=all_red_crop, filename='lfe_objects/SDMs/myrio_test.img',
                         overwrite = T,
                         setting=list(method='weighted',
                                      stat='AUC'))

writeRaster(predict_myrio, filename = "lfe_objects/SDMs/myrio_output.tif",
            overwrite = T)
plot(predict_myrio)
points(myrio_xy)



jpeg(file="lfe_objects/SDMs/myriolecis_wetmorei_CO.jpeg", height = 5,
     width = 6, units = "in",
     res = 600)
plot(predict_myrio)
dev.off()

##### Bacidina inundata #####
Bacidina_inundata = readr::read_csv("lfe_data/SDMs/occurrences/Bacidina_inundata.csv") |>
    dplyr::filter(!is.na(decimalLatitude)) |>
    dplyr::rename(lat = decimalLatitude,
                  lon = decimalLongitude)

names(Bacidina_inundata)

bacid_xy = Bacidina_inundata[, c(75,74)]

bacid_sp <- SpatialPointsDataFrame(coords = bacid_xy, data = Bacidina_inundata,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

plot(clims_crop[[1]])
points(bacid_sp)

library(raster)
bacid_env = raster::extract(all, bacid_sp)

bacid_env = as.data.frame(bacid_env)

# explore
library(ggplot2)

names(bacid_env)

ggplot(bacid_env, aes(x = vapor_pressure_deficit.8)) +
    geom_histogram() +
    theme_minimal()

ggplot(bacid_env, aes(x = current_2.5arcmin_climaticMoistureIndex)) +
    geom_histogram() +
    theme_minimal()

ggplot(bacid_env, aes(x = namerica_v2)) +
    geom_histogram() +
    theme_minimal()

ggplot(bacid_env, aes(x = z)) +
    geom_histogram() +
    theme_minimal()

cor_mat = cor(bacid_env)
cor_mat

# reduce raster stack
names(bacid_env)

#all_red = all[[c(20, 26, 44, 49, 50, 52, 58, 68, 70, 71, 72)]]

bacid_env_red = raster::extract(all_red, bacid_sp)

bacid_env_red = as.data.frame(bacid_env_red)

bacid_env_red$lon = bacid_sp$lon
bacid_env_red$lat = bacid_sp$lat

cor_mat_red = cor(bacid_env_red)
cor_mat_red

bacid_env_red = na.omit(bacid_env_red)
cor_mat_red = cor(bacid_env_red)
cor_mat_red


# option to reduce more!

##### Run SDMs #####

# get background points
backgr = get_backgr(bacid_env_red[c(12,13)], 2000000,
                    1000, all_red[[1]])

# doesn't use raster anymore! terra package.
# backgr = SDMtune::thinData(backgr, all_red[[1]])


backgr = backgr |>
    dplyr::rename(lon = x,
                  lat = y)

bacid_df = rbind(bacid_env_red[,c(12,13)], backgr)
bacid_df$oc = c(rep(1, nrow(bacid_env_red)), rep(0, nrow(backgr)))

coordinates(bacid_df) = ~ lon + lat

d_bacid <- sdmData(formula=oc~., 
                   train=bacid_df, 
                   predictors = all_red)

# fit the models (7 methods, and 10 replications using bootstrapping procedure):
m_bacid <- sdm(oc~.,
               data = d_bacid, 
               methods=c('rf', 'gbm', 'gam'),
               replicatin='boot',
               n=10)

plot(all_red_crop[[1]])

# predict for only colorado to save time
predict_bacid = ensemble(m_bacid, newdata=all_red_crop, filename='lfe_objects/SDMs/bacid_test.img',
                         overwrite = T,
                         setting=list(method='weighted',
                                      stat='AUC'))

writeRaster(predict_bacid, filename = "lfe_objects/SDMs/bacid_output.tif",
            overwrite=T)
plot(predict_bacid)
levelplot(predict_bacid)




jpeg(file="lfe_objects/SDMs/Bacidina_inundata_CO.jpeg", height = 5,
     width = 6, units = "in",
     res = 600)
plot(predict_bacid)
dev.off()

##### Bellemerea sanguinea #####
bellemerea_sanguinea = readr::read_csv("lfe_data/SDMs/occurrences/Bellemerea_sanguinea.csv") |>
    dplyr::filter(!is.na(decimalLatitude)) |>
    dplyr::rename(lat = decimalLatitude,
                  lon = decimalLongitude)

names(bellemerea_sanguinea)

belle_xy = bellemerea_sanguinea[, c(75,74)]

belle_sp <- SpatialPointsDataFrame(coords = belle_xy, data = bellemerea_sanguinea,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

plot(clims_crop[[1]])
points(belle_sp)

library(raster)
belle_env = raster::extract(all, belle_sp)

belle_env = as.data.frame(belle_env)

# explore
library(ggplot2)

names(belle_env)

ggplot(belle_env, aes(x = vapor_pressure_deficit.8)) +
    geom_histogram() +
    theme_minimal()

ggplot(belle_env, aes(x = current_2.5arcmin_climaticMoistureIndex)) +
    geom_histogram() +
    theme_minimal()

ggplot(belle_env, aes(x = namerica_v2)) +
    geom_histogram() +
    theme_minimal()

ggplot(belle_env, aes(x = z)) +
    geom_histogram() +
    theme_minimal()

cor_mat = cor(belle_env)
cor_mat

# reduce raster stack
names(belle_env)

#all_red = all[[c(20, 26, 44, 49, 50, 52, 58, 68, 70, 71, 72)]]

belle_env_red = raster::extract(all_red, belle_sp)

belle_env_red = as.data.frame(belle_env_red)

belle_env_red$lon = belle_sp$lon
belle_env_red$lat = belle_sp$lat

cor_mat_red = cor(belle_env_red)
cor_mat_red

belle_env_red = na.omit(belle_env_red)
cor_mat_red = cor(belle_env_red)
cor_mat_red


# option to reduce more!

##### Run SDMs #####

# get background points
backgr = get_backgr(belle_env_red[c(12,13)], 2000000,
                    1000, all_red[[1]])

# doesn't use raster anymore! terra package.
# backgr = SDMtune::thinData(backgr, all_red[[1]])


backgr = backgr |>
    dplyr::rename(lon = x,
                  lat = y)

belle_df = rbind(belle_env_red[,c(12,13)], backgr)
belle_df$oc = c(rep(1, nrow(belle_env_red)), rep(0, nrow(backgr)))

coordinates(belle_df) = ~ lon + lat

d_belle <- sdmData(formula=oc~., 
                   train=belle_df, 
                   predictors = all_red)

# fit the models (7 methods, and 10 replications using bootstrapping procedure):
m_belle <- sdm(oc~.,
               data = d_belle, 
               methods=c('rf', 'gbm', 'gam'),
               replicatin='boot',
               n=10)


# predict for only colorado to save time
predict_belle = ensemble(m_belle, newdata=all_red_crop, filename='lfe_objects/SDMs/belle_test.img',
                         overwrite = T,
                         setting=list(method='weighted',
                                      stat='AUC'))

writeRaster(predict_belle, filename = "lfe_objects/SDMs/belle_output.tif")
plot(predict_belle)
points(belle_xy)



jpeg(file="lfe_objects/SDMs/bellemerea_sanguinea_CO.jpeg", height = 5,
     width = 6, units = "in",
     res = 600)
plot(predict_belle)
dev.off()

##### Gyalecta fovelaris #####
Gyalecta_foveolaris = readr::read_csv("lfe_data/SDMs/occurrences/Gyalecta_foveolaris.csv") |>
    dplyr::filter(!is.na(decimalLatitude),
                  !is.na(decimalLongitude)) |>
    dplyr::rename(lat = decimalLatitude,
                  lon = decimalLongitude)
Gyalecta_foveolaris
names(Gyalecta_foveolaris)

gyale_xy = na.omit(Gyalecta_foveolaris[, c(75,74)])

gyale_sp <- SpatialPointsDataFrame(coords = gyale_xy, data = Gyalecta_foveolaris,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

plot(clims_crop[[1]])
points(gyale_sp)

library(raster)
gyale_env = raster::extract(all, gyale_sp)

gyale_env = as.data.frame(gyale_env)

# explore
library(ggplot2)

names(gyale_env)

ggplot(gyale_env, aes(x = vapor_pressure_deficit.8)) +
    geom_histogram() +
    theme_minimal()

ggplot(gyale_env, aes(x = current_2.5arcmin_climaticMoistureIndex)) +
    geom_histogram() +
    theme_minimal()

ggplot(gyale_env, aes(x = namerica_v2)) +
    geom_histogram() +
    theme_minimal()

ggplot(gyale_env, aes(x = z)) +
    geom_histogram() +
    theme_minimal()

cor_mat = cor(gyale_env)
cor_mat

# reduce raster stack
names(gyale_env)

#all_red = all[[c(20, 26, 44, 49, 50, 52, 58, 68, 70, 71, 72)]]

gyale_env_red = raster::extract(all_red, gyale_sp)

gyale_env_red = as.data.frame(gyale_env_red)

gyale_env_red$lon = gyale_sp$lon
gyale_env_red$lat = gyale_sp$lat

cor_mat_red = cor(gyale_env_red)
cor_mat_red

gyale_env_red = na.omit(gyale_env_red)
cor_mat_red = cor(gyale_env_red)
cor_mat_red


# option to reduce more!

##### Run SDMs #####

# get background points
backgr = get_backgr(gyale_env_red[c(12,13)], 2000000,
                    100, all_red[[1]])

# doesn't use raster anymore! terra package.
# backgr = SDMtune::thinData(backgr, all_red[[1]])


backgr = backgr |>
    dplyr::rename(lon = x,
                  lat = y)

gyale_df = rbind(gyale_env_red[,c(12,13)], backgr)
gyale_df$oc = c(rep(1, nrow(gyale_env_red)), rep(0, nrow(backgr)))

coordinates(gyale_df) = ~ lon + lat

d_gyale <- sdmData(formula=oc~., 
                   train=gyale_df, 
                   predictors = all_red)

# fit the models (7 methods, and 10 replications using bootstrapping procedure):
m_gyale <- sdm(oc~.,
               data = d_gyale, 
               methods=c('rf', 'gbm', 'gam'),
               replicatin='boot',
               n=10)

# predict for only colorado to save time
predict_gyale = ensemble(m_gyale, newdata=all_red_crop, filename='lfe_objects/SDMs/gyale_test.img',
                         overwrite = T,
                         setting=list(method='weighted',
                                      stat='AUC'))

writeRaster(predict_gyale, filename = "lfe_objects/SDMs/gyale_output.tif")
plot(predict_gyale)
points(gyale_xy)



jpeg(file="lfe_objects/SDMs/Gyalecta_foveolaris_CO.jpeg", height = 5,
     width = 6, units = "in",
     res = 600)
plot(predict_gyale)
dev.off()

##### Xanthoparmelia idahoensis #####
Xanthoparmelia_idahoensis = readr::read_csv("lfe_data/SDMs/occurrences/Xanthoparmelia_idahoensis.csv") |>
    dplyr::filter(!is.na(decimalLatitude)) |>
    dplyr::rename(lat = decimalLatitude,
                  lon = decimalLongitude)

names(Xanthoparmelia_idahoensis)

Xantho_xy = Xanthoparmelia_idahoensis[, c(75,74)]

Xantho_sp <- SpatialPointsDataFrame(coords = Xantho_xy, data = Xanthoparmelia_idahoensis,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

plot(clims_crop[[1]])
points(Xantho_sp)

library(raster)
Xantho_env = raster::extract(all, Xantho_sp)

Xantho_env = as.data.frame(Xantho_env)

# explore
library(ggplot2)

names(Xantho_env)

ggplot(Xantho_env, aes(x = vapor_pressure_deficit.8)) +
    geom_histogram() +
    theme_minimal()

ggplot(Xantho_env, aes(x = current_2.5arcmin_climaticMoistureIndex)) +
    geom_histogram() +
    theme_minimal()

ggplot(Xantho_env, aes(x = namerica_v2)) +
    geom_histogram() +
    theme_minimal()

ggplot(Xantho_env, aes(x = z)) +
    geom_histogram() +
    theme_minimal()

cor_mat = cor(Xantho_env)
cor_mat

# reduce raster stack
names(Xantho_env)

#all_red = all[[c(20, 26, 44, 49, 50, 52, 58, 68, 70, 71, 72)]]

Xantho_env_red = raster::extract(all_red, Xantho_sp)

Xantho_env_red = as.data.frame(Xantho_env_red)

Xantho_env_red$lon = Xantho_sp$lon
Xantho_env_red$lat = Xantho_sp$lat

cor_mat_red = cor(Xantho_env_red)
cor_mat_red

Xantho_env_red = na.omit(Xantho_env_red)
cor_mat_red = cor(Xantho_env_red)
cor_mat_red


# option to reduce more!

##### Run SDMs #####

# get background points
backgr = get_backgr(Xantho_env_red[c(12,13)], 2000000,
                    100, all_red[[1]])

# doesn't use raster anymore! terra package.
# backgr = SDMtune::thinData(backgr, all_red[[1]])


backgr = backgr |>
    dplyr::rename(lon = x,
                  lat = y)

Xantho_df = rbind(Xantho_env_red[,c(12,13)], backgr)
Xantho_df$oc = c(rep(1, nrow(Xantho_env_red)), rep(0, nrow(backgr)))

coordinates(Xantho_df) = ~ lon + lat

d_Xantho <- sdmData(formula=oc~., 
                   train=Xantho_df, 
                   predictors = all_red)

# fit the models (7 methods, and 10 replications using bootstrapping procedure):
m_Xantho <- sdm(oc~.,
               data = d_Xantho, 
               methods=c('rf', 'gbm', 'gam'),
               replicatin='boot',
               n=10)

# predict for only colorado to save time
predict_Xantho = ensemble(m_Xantho, newdata=all_red_crop, filename='lfe_objects/SDMs/Xantho_test.img',
                         overwrite = T,
                         setting=list(method='weighted',
                                      stat='AUC'))

writeRaster(predict_Xantho, filename = "lfe_objects/SDMs/Xantho_output.tif")
plot(predict_Xantho)
points(Xantho_xy)



jpeg(file="lfe_objects/SDMs/Xanthoparmelia_idahoensis_CO.jpeg", height = 5,
     width = 6, units = "in",
     res = 600)
plot(predict_Xantho)
dev.off()
##### Plot #####


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
jpeg(filename = "lfe_objects/SDMs/prelim_SDMs.jpeg", width = 7, height = 5,
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

levelplot(predict_bacid, margin = list(draw = F)) +
    layer(sp.polygons(NF_sp, fill='white', alpha=0.3))

plot(predict_bacid)
plot(NF_sp, bg = "transparent",add = T)


bacid_plot = ggplot() +
    geom_stars(
        data = st_as_stars(predict_bacid),
        aes(x = x, y = y)
    ) +
    scale_fill_continuous(low = "yellow", high = "purple",
                          name = "POC") +
    geom_sf(
        data = NF_reproj, 
        col = "black",
        alpha = 0
    ) +
    xlab("") +
    ylab("") +
    ggtitle("Bacidina inundata") +
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank())

ggsave("lfe_objects/SDMs/bacid_plot.pdf", bacid_plot, device = "pdf",
       width = 10,
       height = 8,
       units = "in")

belle_plot = ggplot() +
    geom_stars(
        data = st_as_stars(predict_belle),
        aes(x = x, y = y)
    ) +
    scale_fill_continuous(low = "yellow", high = "purple",
                          name = "POC") +
    geom_sf(
        data = NF_reproj, 
        col = "black",
        alpha = 0
    ) +
    xlab("") +
    ylab("") +
    ggtitle("Bellemerea sanguinea") +
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank())

ggsave("lfe_objects/SDMs/belle_plot.pdf", belle_plot, device = "pdf",
       width = 10,
       height = 8,
       units = "in")

circi_plot = ggplot() +
    geom_stars(
        data = st_as_stars(predict_circi),
        aes(x = x, y = y)
    ) +
    scale_fill_continuous(low = "yellow", high = "purple",
                          name = "POC") +
    geom_sf(
        data = NF_reproj, 
        col = "black",
        alpha = 0
    ) +
    xlab("") +
    ylab("") +
    ggtitle("Circinaria rogeri") +
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank())

ggsave("lfe_objects/SDMs/circi_plot.pdf", circi_plot, device = "pdf",
       width = 10,
       height = 8,
       units = "in")

gyale_plot = ggplot() +
    geom_stars(
        data = st_as_stars(predict_gyale),
        aes(x = x, y = y)
    ) +
    scale_fill_continuous(low = "yellow", high = "purple",
                          name = "POC") +
    geom_sf(
        data = NF_reproj, 
        col = "black",
        alpha = 0
    ) +
    xlab("") +
    ylab("") +
    ggtitle("Gyalecta foveolaris") +
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank())

ggsave("lfe_objects/SDMs/gyale_plot.pdf", gyale_plot, device = "pdf",
       width = 10,
       height = 8,
       units = "in")

lepro_plot = ggplot() +
    geom_stars(
        data = st_as_stars(predict_lepro),
        aes(x = x, y = y)
    ) +
    scale_fill_continuous(low = "yellow", high = "purple",
                          name = "POC") +
    geom_sf(
        data = NF_reproj, 
        col = "black",
        alpha = 0
    ) +
    xlab("") +
    ylab("") +
    ggtitle("Leprocaulon americanum") +
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank())

ggsave("lfe_objects/SDMs/lepro_plot.pdf", lepro_plot, device = "pdf",
       width = 10,
       height = 8,
       units = "in")

myrio_plot = ggplot() +
    geom_stars(
        data = st_as_stars(predict_myrio),
        aes(x = x, y = y)
    ) +
    scale_fill_continuous(low = "yellow", high = "purple",
                          name = "POC") +
    geom_sf(
        data = NF_reproj, 
        col = "black",
        alpha = 0
    ) +
    xlab("") +
    ylab("") +
    ggtitle("Myriolecis wetmorei") +
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank())

ggsave("lfe_objects/SDMs/myrio_plot.pdf", myrio_plot, device = "pdf",
       width = 10,
       height = 8,
       units = "in")

ompha_plot = ggplot() +
    geom_stars(
        data = st_as_stars(predict_ompha),
        aes(x = x, y = y)
    ) +
    scale_fill_continuous(low = "yellow", high = "purple",
                          name = "POC") +
    geom_sf(
        data = NF_reproj, 
        col = "black",
        alpha = 0
    ) +
    xlab("") +
    ylab("") +
    ggtitle("Omphalora arizonica") +
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank())

ggsave("lfe_objects/SDMs/ompha_plot.pdf", ompha_plot, device = "pdf",
       width = 10,
       height = 8,
       units = "in")

xantho_plot = ggplot() +
    geom_stars(
        data = st_as_stars(predict_Xantho),
        aes(x = x, y = y)
    ) +
    scale_fill_continuous(low = "yellow", high = "purple",
                          name = "POC") +
    geom_sf(
        data = NF_reproj, 
        col = "black",
        alpha = 0
    ) +
    xlab("") +
    ylab("") +
    ggtitle("Xanthoparmelia idahoensis") +
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank())

ggsave("lfe_objects/SDMs/xantho_plot.pdf", xantho_plot, device = "pdf",
       width = 10,
       height = 8,
       units = "in")

all8 = predict_bacid + predict_belle +
    predict_circi + predict_gyale +
    predict_lepro + predict_myrio +
    predict_ompha + predict_Xantho


all_plot = ggplot() +
    geom_stars(
        data = st_as_stars(all8),
        aes(x = x, y = y)
    ) +
    scale_fill_continuous(low = "yellow", high = "purple",
                          name = "POC") +
    geom_sf(
        data = NF_reproj, 
        col = "black",
        alpha = 0
    ) +
    xlab("") +
    ylab("") +
    ggtitle("All 8") +
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank())
all_plot

ggsave("lfe_objects/SDMs/all8_plot.pdf", all_plot, device = "pdf",
       width = 10,
       height = 8,
       units = "in")
