##### Lichen Functional Traits #####
source("header.R")

#' Pseudocode:
#' 
#' Load in all data from various spreadsheets:
#' 
#' Reflectance spectra from OceanView files
#' Absorbance spectra from Nanodrop
#' Water relations spreadsheet
#' Hydrophobicity
#' Cross Sections
#' 
#' Load in Plot data:
#' Occurrence
#' Microclimate
#' Position
#' Relative Abundance
#' Canopy Cover
#' 
#' Merge datasets and do exploratory data analysis.
#' corplots
#' PCAs

##### Load in data #####
## Reflectance spectra

# load in ids
reflect_ids = read.csv("lfe_data/functional_traits/reflect_ids.csv")
reflect_ids = reflect_ids |>
    dplyr::rename(num = Reflectance.Spectrometry.Nums,
                  collection = Collection.Num,
                  plot = Plot.Num,
                  species = Species)
reflect_ids$sample = paste0(tolower(substr(reflect_ids$species,1,3)), 
                            substr(sub('.* ', '', reflect_ids$species), 1, 3), "_",
                            reflect_ids$plot, "_", reflect_ids$collection)

# list folders
filelist = list.files("lfe_data/functional_traits/reflect/",
           pattern = "*.txt", recursive = TRUE)
filelist = paste0("lfe_data/functional_traits/reflect/", filelist)
filelist


datalist = lapply(filelist, function(x)as.data.frame(fread(x, header=F, sep = "\t")))
lichen_spectra = do.call("rbind", datalist) |>
    dplyr::filter(V1 > 250 & V1 < 800)
# add id number
lichen_spectra$num = rep(1:length(datalist), each = 1607)
lichen_spectra$path = rep(filelist, each = 1607)
lichen_spectra = lichen_spectra |>
    separate_wider_delim(cols = path,
                         names = c("1", "2", "3", "Folder", "file"),
                         delim = "/")
lichen_spectra$file = substr(lichen_spectra$file, 1,
                                  nchar(lichen_spectra$file)-4)
lichen_spectra$num = as.numeric(substr(lichen_spectra$file, 
                            nchar(lichen_spectra$file)-1,
                            nchar(lichen_spectra$file)))
str(lichen_spectra)
head(lichen_spectra)

lichen_spectra = merge(lichen_spectra, reflect_ids, by = c("num", "Folder"))
lichen_spectra = lichen_spectra |>
    dplyr::rename(wavelength = V1,
                  reflectance = V2) |>
    dplyr::mutate(sample_state = paste(sample, State, sep="_")) |>
    dplyr::select(-c("1","2", "3"))

# take mean of all three spectra per specimen
lichen_spectra_sum = lichen_spectra |>
    dplyr::group_by(sample_state, wavelength) |>
    dplyr::summarise(mean_reflectance = mean(reflectance), across())

# take mean of each integer wavelength to reduce data
lichen_spectra_sum = lichen_spectra_sum |>
    dplyr::mutate(wavelength_int = as.numeric(substr(wavelength, 1, 3))) |>
    group_by(sample_state, wavelength_int) |>
    dplyr::summarise(mean_reflectance = mean(reflectance)) |>
    dplyr::ungroup() |>
    separate(sample_state, into = c("species", "plot", "collection",
                                    "state"), sep = "_",
             remove = F) |>
    dplyr::mutate(sample = substr(sample_state, 1, nchar(sample_state)-4))

spectra_plot = ggplot(data = lichen_spectra_sum,
                      mapping = aes(x = wavelength_int,
                                    y = mean_reflectance,
                                    group = sample_state,
                                    color = species,
                                    linetype = state)) +
    geom_line() +
    theme_minimal()
spectra_plot


ggsave("lfe_objects/functional_traits/spectra_plot.jpeg", plot = spectra_plot,
       dpi = 600, width = 7, height = 5, units = "in")

## Absorbance spectra

# load in data
filelist_CH = list.files(path = "lfe_data/functional_traits/Nanodrop/CH", pattern = "*.tsv")
filelist_CH = paste0("lfe_data/functional_traits/Nanodrop/CH/", filelist_CH)
filelist_CH

filelist_SM = list.files(path = "lfe_data/functional_traits/Nanodrop/SM/new", pattern = "*.tsv")
filelist_SM = paste0("lfe_data/functional_traits/Nanodrop/SM/new/", filelist_SM)
filelist_SM

# Chlorophyll
CH = lapply(filelist_CH, function(x)as.data.frame(fread(x, header=T, sep = "\t"))) |>
    reduce(full_join, by = NULL)
names(CH)[6] = "666"
CH = CH[ -c(3,5) ]


CH = pivot_longer(CH, cols = c(4:ncol(CH)),
                                    names_to = "wavelength",
                                    values_to = "absorbance")
CH$wavelength = as.numeric(CH$wavelength)
CH = CH |>
    dplyr::rename(sample = "Sample ID",
                  date = "Date and Time") |>
    dplyr::filter(wavelength > 260 &
                wavelength < 745) |>
    separate(col = sample, into = c("species","plot", "collection"), 
             sep = "_", remove = F)

CH_plot = ggplot(data = CH, 
                 mapping = aes(x = wavelength,
                            y = absorbance,
                            group = sample,
                            color = species)) +
    geom_line() +
    theme_minimal() +
    #xlim(400, 680) +
    #ylim(0, 1.25) +
    geom_vline(xintercept = 665) +
    geom_vline(xintercept = 648) +
    geom_vline(xintercept = 435) +
    geom_vline(xintercept = 415) +
    annotate(geom = "text", x = 475, y = 3, label = "Phaeophytinization\nQuotient\n 435 / 415") +
    annotate(geom = "text", x = 650, y = 2, label = "Chlorophyll a & b") +
    ggtitle("Chlorophyll Extractions")
CH_plot


ggsave("lfe_objects/functional_traits/CH_plot.jpeg", plot = CH_plot,
       dpi = 600, width = 7, height = 5, units = "in")

# Secondary Metabolites
SM = lapply(filelist_SM, function(x)as.data.frame(fread(x, header=T, sep = "\t"))) |>
    reduce(full_join, by = NULL)
names(SM)[6] = "290"
SM = SM[ -c(3,5) ]

SM = pivot_longer(SM, cols = c(4:ncol(SM)),
                  names_to = "wavelength",
                  values_to = "absorbance")
SM$wavelength = as.numeric(SM$wavelength)
SM = SM |>
    dplyr::rename(sample = "Sample ID",
                  date = "Date and Time") |>
    dplyr::filter(wavelength > 260 &
                      wavelength < 400) |>
    separate(col = sample, into = c("species","plot", "collection"), 
             sep = "_", remove = T)
SM$species = ifelse(SM$species == "mansax", "monsax", SM$species)
SM$sample = paste0(tolower(substr(SM$species,1,6)), 
                    "_",
                            SM$plot, "_", SM$collection)

SM_plot = ggplot(data = SM, 
                 mapping = aes(x = wavelength,
                               y = absorbance,
                               group = sample,
                               color = species)) +
    geom_line() +
    theme_minimal()
SM_plot


ggsave("lfe_objects/functional_traits/SM_plot.jpeg", plot = SM_plot,
       dpi = 600, width = 7, height = 5, units = "in")

## Water relations and tissue investment
water = read.csv("lfe_data/functional_traits/water_relations.csv") |>
    dplyr::rename(plot = Plot.Num,
                  collection = Collection.Num,
                  species = Species)

# add sample identifier
water$sample = paste0(tolower(substr(water$species,1,3)), 
                            substr(sub('.* ', '', water$species), 1, 3), "_",
                            water$plot, "_", water$collection)

# calculate specific thallus mass and water holding capacity
water$STM = water$Dry_Weight / water$Area_mm2    
water$WHC = (((water$Wet_Weight - water$Dry_Weight) / water$Dry_Weight) * 100)


STM_plot = ggplot(data = water,
                  mapping = aes(x = species,
                                y = STM)) +
    geom_boxplot() +
    theme_minimal()
STM_plot

WHC_plot = ggplot(data = water,
                  mapping = aes(x = species,
                                y = WHC)) +
    geom_boxplot() +
    theme_minimal()
WHC_plot

ggsave("lfe_objects/functional_traits/STM.jpeg", plot = STM_plot,
       dpi = 600, width = 7, height = 5, units = "in")

ggsave("lfe_objects/functional_traits/WHC.jpeg", plot = WHC_plot,
       dpi = 600, width = 7, height = 5, units = "in")

##### Drying Curves #####


# add columns needed later
water$ww = water$Wet_Weight


# pivot longer
water_pivot = pivot_longer(water, cols = c(9:20),
                           names_to = "time",
                           values_to = "weight")

# rename time column values to numbers
water_pivot$time = ifelse(water_pivot$time == "Wet_Weight", 0,
                ifelse(water_pivot$time == "X3_Min", 3,
                ifelse(water_pivot$time == "X6_Min", 6,
                ifelse(water_pivot$time == "X9_Min", 9,
                ifelse(water_pivot$time == "X12_Min", 12,
                ifelse(water_pivot$time == "X15_Min", 15,
                ifelse(water_pivot$time == "X18_Min", 18,
                ifelse(water_pivot$time == "X21_Min", 21,
                ifelse(water_pivot$time == "X24_Min", 24,
                ifelse(water_pivot$time == "X27_Min", 27,
                ifelse(water_pivot$time == "X30_Min", 30,
                ifelse(water_pivot$time == "X33_Min", 33,
                NA))))))))))))
str(water_pivot)

# add relative water content
water_pivot$RWC = (water_pivot$weight - water_pivot$Dry_Weight) /
                    (water_pivot$ww - water_pivot$Dry_Weight) * 100

# plot
drying_curves = ggplot(data = water_pivot,
                       mapping = aes(x = time,
                                     y = RWC,
                                     color = species)) +
    geom_jitter(width = .4) +
    #geom_line(mapping = aes(group = sample)) +
    geom_smooth() +
    theme_minimal() +
    ylim(0, 100)

drying_curves

ggsave("lfe_objects/functional_traits/dry_curve.jpeg", plot = drying_curves,
       dpi = 600, width = 10, height = 5, units = "in")

# save dataframe
write.csv(water_pivot, "lfe_objects/functional_traits/drying_curves.csv")

# calculate water loss per time
water$W9 = (((water$X9_Min - water$Dry_Weight) / water$Dry_Weight) / 
                water$WHC) * 
                100

water$WL = (1 - water$W9) / 9

ggplot(data = water,
       mapping = aes(x = species,
                     y = WL)) +
    geom_boxplot()

## Cross Sections

CS = read.csv("lfe_data/functional_traits/cross_sections.csv") |>
    dplyr::rename(thickness = Layer.Thickness,
                  tissue = Tissue.Type,
                  imgnum = Image.Number)
CS = CS[-c(2:7)]

# read in IDs dataset
CS_ids = read.csv("lfe_data/functional_traits/cross_section_ids.csv") |>
    separate(Cross.Section.Pic.Nums, 
                    into = c("pic1", "pic2", "pic3")) |>
    dplyr::rename(species = Species,
                  collection = Collection.Num,
                  plot = Plot.Num)
CS_ids$sample = paste0(tolower(substr(CS_ids$species,1,3)), 
                      substr(sub('.* ', '', CS_ids$species), 1, 3), "_",
                      CS_ids$plot, "_", CS_ids$collection)

CS_ids_pivot = pivot_longer(CS_ids, cols = c(4:6),
                            names_to = "img",
                            values_to = "imgnum")

CS = merge(CS, CS_ids_pivot, by = "imgnum")
CS_sum = CS |>
    dplyr::group_by(sample, tissue, plot, collection, species) |>
    dplyr::summarise(mean_thickness = mean(thickness)) |>
    dplyr::ungroup()

CS_plot = ggplot(data = CS_sum,
       mapping = aes(x = tissue,
                     y = mean_thickness,
                     color = species)) +
    geom_boxplot() +
    theme_bw()
CS_plot

ggsave("lfe_objects/functional_traits/CS_plot.jpeg", plot = CS_plot,
       dpi = 600, width = 7, height = 5, units = "in")

CS_pivot = pivot_wider(CS_sum, 
                     names_from = tissue, 
                     values_from = mean_thickness)


## load in plot chars full dataset
plot_chars = read.csv("lfe_objects/OSMP_plots/plot_chars_full.csv")

## load in collection dataset
species_list = c("Flavopunctelia soredica",
                 "Parmelia sulcata",
                 "Montanelia saximontana",
                 "Umbilicaria americana",
                 "Umbilicaria hyperborea")

collections = read.csv("lfe_data/OSMP_plots/plot_collections_3_15_24.csv") |>
    dplyr::mutate(species = paste(Genus, Specific.Epithet)) |>
    dplyr::filter(species %in% species_list) |>
    dplyr::rename(plot = Plot,
                  collection = Collection.Number,
                  collection_date = Date)

# add sample identifier
collections$sample = paste0(tolower(substr(collections$species,1,3)), 
                      substr(sub('.* ', '', collections$species), 1, 3), "_",
                      collections$plot, "_", collections$collection)

##### Summarise and Merge Datasets ##### 

## Summarise Reflectance spectra
# calculate spectral indices from bartak et al. 2016
spectral_indices = lichen_spectra_sum |>
    dplyr::group_by(sample_state, sample, state, plot, collection, species) |>
    dplyr::summarise(MCARI1 = 1.2 * (2.5 * 
                (mean_reflectance[which(wavelength_int == 790)] - 
                     mean_reflectance[which(wavelength_int == 670)]) - 1.3 *
                    (mean_reflectance[which(wavelength_int == 790)] - 
                         mean_reflectance[which(wavelength_int == 550)])),
                MCARI = ((mean_reflectance[which(wavelength_int == 700)] -
                         mean_reflectance[which(wavelength_int == 670)]) -
                        0.2 *
                    (mean_reflectance[which(wavelength_int == 700)] -
                     mean_reflectance[which(wavelength_int == 550)])) *
                    (mean_reflectance[which(wavelength_int == 700)] /
                     mean_reflectance[which(wavelength_int == 670)]),
                SRPI = mean_reflectance[which(wavelength_int == 430)] /
                    mean_reflectance[which(wavelength_int == 680)],
    PRI = (mean_reflectance[which(wavelength_int == 531)] -
               mean_reflectance[which(wavelength_int == 570)]) /
    (mean_reflectance[which(wavelength_int == 531)] +
         mean_reflectance[which(wavelength_int == 570)]),
    NPCI = (mean_reflectance[which(wavelength_int == 680)] -
           mean_reflectance[which(wavelength_int == 430)]) /
    (mean_reflectance[which(wavelength_int == 680)] +
         mean_reflectance[which(wavelength_int == 430)]),
LI = (mean_reflectance[which(wavelength_int == 790)] -
           mean_reflectance[which(wavelength_int == 680)]) /
    (mean_reflectance[which(wavelength_int == 790)] +
         mean_reflectance[which(wavelength_int == 680)]),
NDVI = (mean_reflectance[which(wavelength_int == 799)] -
          mean_reflectance[which(wavelength_int == 700)]) /
    (mean_reflectance[which(wavelength_int == 799)] +
         mean_reflectance[which(wavelength_int == 700)]),
UV_ref = mean(mean_reflectance[which(wavelength_int > 260 & wavelength_int < 325)]),
NIR_ref = mean(mean_reflectance[which(wavelength_int > 750 & wavelength_int < 799)])) |>
    dplyr::ungroup()

# Bartak et al 2016
# MCARI1 = 1.2 * (2.5 * (R790 - R670) - 1.3 * (R790 - R550))
# MCARI = ((R700- R670) - 0.2 * (R700- R550)) * (R700/ R670)
# SRPI = R430 / R680
# PRI = (R531- R570) / (R531 + R570)
# NPCI = (R680- R430) / (R680+ R430)
# LI = (R790 - R680) / (R790 + R680)
# NDVI (R799 - R700) / (R799 + R700)

## Absorbance spectra
SM_sum = SM |>
    dplyr::group_by(sample, species, plot, collection) |>
    dplyr::summarise(UV_abs = mean(absorbance[which(wavelength > 260 & wavelength < 325)])) |>
    dplyr::ungroup()

# Barnes et al., 1992 Phaeophytinization quotient
# PQ = 435 / 415
# optimum = 1.41 in pure chlorophyll
# minimum of 0.56
# add 665 and 648 based using weighted means of neighboring values
# chlorophyll concentrations are in micrograms per milliliter 
CH_sum = CH |>
    dplyr::group_by(sample, species, plot, collection) |>
    dplyr::summarise(A435 = ((2 * absorbance[which(wavelength == 436)]) + 
                                 absorbance[which(wavelength == 433)]) / 3,
                     A415 = absorbance[which(wavelength == 415)],
                     A665 = mean(c(absorbance[which(wavelength == 666)],
                                   absorbance[which(wavelength == 664)])),
                     A648 = ((2 * absorbance[which(wavelength == 649)]) + 
                                 absorbance[which(wavelength == 646)]) / 3,
                     CH_UV_abs = mean(absorbance[which(wavelength > 260 & wavelength < 325)])) |>
    dplyr::mutate(PQ = A435 / A415,
                  CHa = (14.85 * A665) - (5.14 * A648),
                  CHb = (25.48 * A648) - (7.36 * A665),
                  CH = (7.49 * A665) + (20.34 * A648)) |>
    dplyr::ungroup()

## Merge all datasets!

# add chlorophyll dataset
ft_plot = merge(plot_chars |>
                    dplyr::select(!c(Date)), CH_sum, by = "plot")

# Secondary metabolites
ft_plot = merge(ft_plot, SM_sum |>
                    dplyr::select(!c(species, collection)),
                by = "sample")

# add mean UV_abs value from chlorophyll and SM measurements
ft_plot$mean_UV_abs = (ft_plot$UV_abs + ft_plot$CH_UV_abs) / 2

# spectral indices
ft_plot = merge(ft_plot, spectral_indices |>
                    dplyr::select(!c(species, collection, plot)), 
                by = "sample")

# cross sections
ft_plot = merge(ft_plot, CS_pivot |>
                    dplyr::select(!c(species, collection, plot)),
                by = "sample")

## add total thallus thickness
ft_plot$thallus_thickness = ft_plot$`Upper Cortex` + ft_plot$Algal +
    ft_plot$Medulla + ft_plot$`Lower Cortex`

# water
ft_plot = merge(ft_plot, water |>
                    dplyr::select(!c(species, collection, plot, Date)),
                by = "sample")

# collection data
ft_plot = merge(ft_plot, collections |>
                    dplyr::select(!c(Unique.ID, plot, collection,
                                     Genus, Specific.Epithet,
                                     Determination, Det.Date,
                                     Reflectance.Spectroscopy,
                                     Photo.Voucher,
                                     species)),
                by = "sample")
ft_plot$coll_northing = as.numeric(lapply(ft_plot$Aspect.y, northing))

# divide chlorophyll and SM by weight and area!
ft_plot$CH_area = ft_plot$CH / ft_plot$Area_mm2
ft_plot$CH_weight = ft_plot$CH / ft_plot$Dry_Weight

ft_plot$SM_area = ft_plot$UV_abs / ft_plot$Area_mm2
ft_plot$SM_weight = ft_plot$UV_abs / ft_plot$Dry_Weight

ft_plot$mean_SM_area = ft_plot$mean_UV_abs / ft_plot$Area_mm2
ft_plot$mean_SM_weight = ft_plot$mean_UV_abs / ft_plot$Dry_Weight

# save processed dataset
write.csv(ft_plot, "lfe_objects/functional_traits/ft_plot.csv")

