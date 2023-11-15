##### Lichen reflectance spectra data #####

# libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(photobiology)
library(grid)

# setwd
setwd("~/R/lichen_spectra")
filelist = list.files(path = "~/R/lichen_spectra/spectra", pattern = "*.txt")
filelist = paste0("~/R/lichen_spectra/spectra/", filelist)
filelist
#assuming tab separated values with a header    
datalist = lapply(filelist, function(x)as.data.frame(fread(x, header=F, sep = "\t")))
lichen_spectra = do.call("rbind", datalist) %>%
    rename(
        wavelength = V1,
        reflectance = V2
    ) %>%
    mutate(
        sample = as.factor(rep(1:length(datalist), each = nrow(datalist[[1]])))) %>%
    filter(wavelength > 250 & wavelength < 800)


gradient <- t(w_length2rgb(250:800))
g <- rasterGrob(gradient, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE) 

test = ggplot(data = lichen_spectra, aes(x = wavelength, y = reflectance, color = sample)) +
    theme_bw() +
    annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
    geom_line(size = 1.25) +
    scale_x_continuous(breaks = seq(300, 800, 200)) +
    labs(x = "Wavelength (nm)", y = "Reflectance (%)") +
    theme(
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        legend.position = "none"
    )
test
ggsave("output.jpeg", plot = test, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)   

spectra = read.csv("spectra/spectrometry_test.csv")

spectra = spectra |>
    dplyr::rename(reflectance_absorbance = relfectance_absorbance) |>
    dplyr::filter(wavelength > 250)

physcia = spectra |>
    dplyr::filter(species == "Physcia_stellaris",
                  type == "reflectance")

physcia_plot = ggplot(data = physcia, aes(x = wavelength, y = reflectance_absorbance, color = dry_wet)) +
    theme_bw() +
    annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
    geom_line(size = 1.5) +
    scale_x_continuous(breaks = seq(300, 800, 200)) +
    labs(x = "Wavelength (nm)", y = "Reflectance (%)") +
    theme(
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        legend.position = "none"
    )
physcia_plot
ggsave("physcia_stellaris_reflect.jpeg", plot = physcia_plot, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)   


physcia = spectra |>
    dplyr::filter(species == "Physcia_stellaris",
                  type == "absorbance")

physcia_plot = ggplot(data = physcia, aes(x = wavelength, y = reflectance_absorbance, color = dry_wet)) +
    theme_bw() +
    annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
    geom_line(size = 1.25) +
    scale_x_continuous(breaks = seq(300, 800, 200)) +
    labs(x = "Wavelength (nm)", y = "Absorbance") +
    theme(
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        legend.position = "none"
    )
physcia_plot

unique(spectra$species)


melanohalea = spectra |>
    dplyr::filter(species == "Melanohalea_soridia",
                  type == "reflectance")

melanohalea_plot = ggplot(data = melanohalea, aes(x = wavelength, y = reflectance_absorbance, color = dry_wet)) +
    theme_bw() +
    annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
    geom_line(size = 1.5) +
    scale_x_continuous(breaks = seq(300, 800, 200)) +
    labs(x = "Wavelength (nm)", y = "Reflectance (%)") +
    theme(
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        legend.position = "none"
    )
melanohalea_plot
ggsave("Melanohalea_reflect.jpeg", plot = melanohalea_plot, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)   


dimelaena = spectra |>
    dplyr::filter(species == "Dimelaena_oreina",
                  type == "reflectance")

dimelaena_plot = ggplot(data = dimelaena, aes(x = wavelength, y = reflectance_absorbance, color = dry_wet)) +
    theme_bw() +
    annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
    geom_line(size = 1.5) +
    scale_x_continuous(breaks = seq(300, 800, 200)) +
    labs(x = "Wavelength (nm)", y = "Reflectance (%)") +
    theme(
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        legend.position = "none"
    )
dimelaena_plot

ggsave("dimelaena_reflect.jpeg", plot = dimelaena_plot, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)   


calicium = spectra |>
    dplyr::filter(species == "Buellia_neon",
                  type == "reflectance")

calicium_plot = ggplot(data = calicium, aes(x = wavelength, y = reflectance_absorbance, color = dry_wet)) +
    theme_bw() +
    annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
    geom_line(size = 1.5) +
    scale_x_continuous(breaks = seq(300, 800, 200)) +
    labs(x = "Wavelength (nm)", y = "Reflectance (%)") +
    theme(
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        legend.position = "none"
    )
calicium_plot
ggsave("Calicium_reflect.jpeg", plot = calicium_plot, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)   


peltigera = spectra |>
    dplyr::filter(species == "Peltigera_grey",
                  type == "reflectance")

peltigera_plot = ggplot(data = peltigera, aes(x = wavelength, y = reflectance_absorbance, color = dry_wet)) +
    theme_bw() +
    annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
    geom_line(size = 1.25) +
    scale_x_continuous(breaks = seq(300, 800, 200)) +
    labs(x = "Wavelength (nm)", y = "Reflectance (%)") +
    theme(
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        legend.position = "none"
    )
peltigera_plot

physcia2 = spectra |>
    dplyr::filter(species == "Physcia_marginal_soridia",
                  type == "reflectance")

physcia2_plot = ggplot(data = physcia2, aes(x = wavelength, y = reflectance_absorbance, color = dry_wet)) +
    theme_bw() +
    annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
    geom_line(size = 1.25) +
    scale_x_continuous(breaks = seq(300, 800, 200)) +
    labs(x = "Wavelength (nm)", y = "Reflectance (%)") +
    theme(
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        legend.position = "none"
    )
physcia2_plot



