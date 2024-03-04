##### Topography #####
# guide to using r as GIS for ecologists
# https://tmieno2.github.io/R-as-GIS-for-Economists/index.html


source("header.R")

##### Larger scale DEM - 30 M resolution ##### 
# load in DEMs for 40lat 107lon - NW, 40lat 106lon - NE,
# 39lat 107lon - SW, 39lat 106lon - SE

NNE = raster("lfe_data/SDMs/CO_DEM/USGS_1_n41w106_20230314.tif")
NNW = raster("lfe_data/SDMs/CO_DEM/USGS_1_n41w107_20230314.tif")
NNE = raster("lfe_data/SDMs/CO_DEM/USGS_1_n41w106_20230314.tif")
NW = raster("lfe_data/SDMs/CO_DEM/USGS_1_n40w107_20220216.tif")
NE = raster("lfe_data/SDMs/CO_DEM/USGS_1_n40w106_20230602.tif")
SW = raster("lfe_data/SDMs/CO_DEM/USGS_1_n39w107_20220331.tif")
SE = raster("lfe_data/SDMs/CO_DEM/USGS_1_n39w106_20230602.tif")

# load in plot characteristics
plot_chars = read.csv("lfe_objects/OSMP_plots/plot_chars_full.csv")

# merge DEMs
DEM = merge(NNW, NNE, NW, NE, SW, SE)
plot(DEM)

# crop
OSMP_extent = extent(c(-105.335, -105.25, 39.91, 40.125))
DEM = crop(DEM, OSMP_extent)
plot(DEM)
points(plot_chars$Lon, plot_chars$Lat)

# convert to stars
DEM = st_as_stars(DEM)
OSMP_plots = ggplot() +
    geom_stars(data = DEM) +
    coord_equal() +
    geom_point(data = plot_chars,
               mapping = aes(x = Lon,
                             y = Lat))
OSMP_plots

st_crs(DEM)

# transform from NAD83 to WGS84 (WKT/EPSG code for WGS84 = 4326)
DEM = st_warp(DEM, crs = st_crs(4326))
st_crs(DEM)

OSMP_plots = ggplot() +
    geom_stars(data = DEM) +
    coord_equal() +
    geom_point(data = plot_chars,
               mapping = aes(x = Lon,
                             y = Lat))

OSMP_plots
#plot(DEM)
#points(plot_chars$Lon, plot_chars$Lat)

# crop to OSMP boundaries
# create bounding box to crop to
#bb = st_bbox(c(xmin = -105.4,
#               ymin = 39.78,
#               xmax = -105,
#               ymax = 40.26), crs = st_crs(DEM))
#plot(bb)

#DEM_crop = st_crop(DEM, bb)

#plot(DEM_crop)

##### Smaller Scale DEM - 10M Resolution #####

# OpenTopography Job
# https://portal.opentopography.org/rasterOutput?jobId=rt1701800748674
# metadata
# https://portal.opentopography.org/rasterOutput?jobId=rt1701800748674&metadata=1

# untar the files
#untar("lfe_data/SDMs/OSMP_OpenTopography/dinfSlope.tar.gz", exdir = "lfe_data/SDMs/OSMP_OpenTopography")
#untar("lfe_data/SDMs/OSMP_OpenTopography/rasters_USGS10m.tar.gz", exdir = "lfe_data/SDMs/OSMP_OpenTopography")
#untar("lfe_data/SDMs/OSMP_OpenTopography/TWI.tar.gz", exdir = "lfe_data/SDMs/OSMP_OpenTopography")
#untar("lfe_data/SDMs/OSMP_OpenTopography/viz.tar.gz", exdir = "lfe_data/SDMs/OSMP_OpenTopography")
#untar("lfe_data/SDMs/OSMP_OpenTopography/CHM.tar.gz", exdir = "lfe_data/SDMs/OSMP_OpenTopography")


# load in files
OSMP_DEM = raster("lfe_data/SDMs/OSMP_OpenTopography/output_USGS10m.tif")
OSMP_TWI = raster("lfe_data/SDMs/OSMP_OpenTopography/TWItwi.tif")
OSMP_aspect = raster("lfe_data/SDMs/OSMP_OpenTopography/viz.USGS10m_aspect.tif")
OSMP_rough = raster("lfe_data/SDMs/OSMP_OpenTopography/viz.USGS10m_roughness.tif")
OSMP_slope = raster("lfe_data/SDMs/OSMP_OpenTopography/viz.USGS10m_slope.tif")
OSMP_CHM = raster("lfe_data/SDMs/OSMP_OpenTopography/CHM.tif")

plot(OSMP_CHM)
# chack that slope and aspect are in degrees and DEM in meters
cellStats(OSMP_slope, "max")
cellStats(OSMP_aspect, "max")
cellStats(OSMP_DEM, "max")
cellStats(OSMP_TWI, "max")
cellStats(OSMP_rough, "max")

# crop to OSMP area
OSMP_DEM = crop(OSMP_DEM, OSMP_extent)
OSMP_TWI = crop(OSMP_TWI, OSMP_extent)
OSMP_aspect = crop(OSMP_aspect, OSMP_extent)
OSMP_rough = crop(OSMP_rough, OSMP_extent)
OSMP_slope = crop(OSMP_slope, OSMP_extent)

plot(OSMP_DEM)
points(plot_chars$Lon, plot_chars$Lat)

# convert to stars
OSMP_DEM = st_as_stars(OSMP_DEM)
OSMP_TWI = st_as_stars(OSMP_TWI)
OSMP_aspect = st_as_stars(OSMP_aspect)
OSMP_rough = st_as_stars(OSMP_rough)
OSMP_slope = st_as_stars(OSMP_slope)

# convert from NAD83 to WGS84
st_crs(OSMP_DEM)
OSMP_DEM = st_warp(OSMP_DEM, crs = st_crs(4326))
st_crs(OSMP_DEM)

OSMP_TWI = st_warp(OSMP_TWI, crs = st_crs(4326))
OSMP_aspect = st_warp(OSMP_aspect, crs = st_crs(4326))
OSMP_rough = st_warp(OSMP_rough, crs = st_crs(4326))
OSMP_slope = st_warp(OSMP_slope, crs = st_crs(4326))

# plot
ggplot() +
    geom_stars(data = OSMP_DEM) +
    coord_equal()
ggplot() +
    geom_stars(data = OSMP_TWI) +
    coord_equal()
ggplot() +
    geom_stars(data = OSMP_aspect) +
    coord_equal()
ggplot() +
    geom_stars(data = OSMP_rough) +
    coord_equal()
ggplot() +
    geom_stars(data = OSMP_slope) +
    coord_equal()
ggplot() +
    geom_stars(data = OSMP_CHM) +
    coord_equal()

##### Convert OSMP Vegetation Shapefile to Raster #####

# read in shapefile
OSMP_veg = st_read("lfe_data/SDMs/OSMP_Veg_USNVC_Alliances")

# find right level of vegetation to use
# target types
# 1: Shrub/Grassland/Wetland
# 2: Open Ponderosa Pine Forest
# 3: Douglas Fir/Mixed Forest
# 4: Cliffs/Talus
unique(OSMP_veg$MACROGROUP)
unique(OSMP_veg$SUBCLASS)
unique(OSMP_veg$CONSTARGET)




##### Canopy Openness from CHM #####

##### Prepare data for meteoland #####



