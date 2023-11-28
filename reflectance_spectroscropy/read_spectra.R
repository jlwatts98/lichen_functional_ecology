##### Lichen reflectance spectra data #####

# header
source("header.R")


filelist = list.files(path = "lfe_data/reflectance_spectroscopy/test", pattern = "*.txt")
filelist = paste0("lfe_data/reflectance_spectroscopy/test/", filelist)
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
ggsave("lfe_objects/reflectance_spectroscopy/test.jpeg", 
       plot = test, 
       device = "jpeg", 
       width = 8, 
       height = 6, 
       units = "in", 
       dpi = 600)   

spectra = read.csv("lfe_data/reflectance_spectroscopy/spectrometry_test.csv")

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
ggsave("lfe_objects/reflectance_spectroscopy/physcia_stellaris_reflect.jpeg", plot = physcia_plot, device = "jpeg", width = 8, height = 6, units = "in", dpi = 300)   


physcia = spectra |>
    dplyr::filter(species == "Physcia_stellaris",
                  type == "absorbance")

physcia_plot1 = ggplot(data = physcia, aes(x = wavelength, y = reflectance_absorbance, color = dry_wet)) +
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
physcia_plot1

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
ggsave("lfe_objects/reflectance_spectroscopy/Melanohalea_reflect.jpeg", 
       plot = melanohalea_plot, 
       device = "jpeg", 
       width = 8, 
       height = 6, 
       units = "in", 
       dpi = 600)   


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

ggsave("lfe_objects/reflectance_spectroscopy/dimelaena_reflect.jpeg", 
       plot = dimelaena_plot, 
       device = "jpeg", 
       width = 8, 
       height = 6, 
       units = "in", 
       dpi = 600)   


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
ggsave("lfe_objects/reflectance_spectroscopy/Calicium_reflect.jpeg", 
       plot = calicium_plot, device = "jpeg", 
       width = 8, height = 6, units = "in", dpi = 600)   


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

# arrange plots
refl_plot = ggarrange(physcia_plot, melanohalea_plot,
                      dimelaena_plot, calicium_plot, 
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2,
                    font.label = list(size = 30))
refl_plot

ggsave("lfe_objects/reflectance_spectroscopy/foursp.jpeg", 
       plot = refl_plot, device = "jpeg", 
       width = 16, height = 12, units = "in", dpi = 600) 


### Flavopunctelia soredica by plot

# load in data
colxn = read_csv("lfe_data/OSMP_plots/plot_collections_11_16_23.csv") |>
    rename(plot = Plot)

# load in plot characteristics
plot_chars = read_csv("lfe_objects/OSMP_plots/plot_chars_full.csv")

# merge dfs
df = merge(colxn, plot_chars, by = "plot")
# load in F soredica reflectance spectra
f_sore_refl = read_csv("lfe_data/reflectance_spectroscopy/F_soredica_reflectance.csv")

f_sore_red = f_sore_refl |>
    dplyr::filter(probe_distance == "far",
                  Wavelength > 250 & Wavelength < 800,
                  `Plot Number` != 15 & `Collection Number` != 4) |>
    dplyr::rename(plot = `Plot Number`)

f_sore_all = merge(f_sore_red, plot_chars, by = "plot")


f_sore_plot = f_sore_all |>
    dplyr::filter(wet_dry == "dry") |>
                  ggplot( 
                     aes(x = Wavelength, 
                         y = Reflectance,
                         group = plot,
                         color = canopy_cover)) +
    theme_bw() +
    annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
    geom_line(size = 0.65) +
    scale_x_continuous(breaks = seq(300, 800, 200)) +
    scale_color_continuous(type = "viridis") +
    labs(x = "Wavelength (nm)", y = "Reflectance (%)") +
    theme(
        legend.position = "none"
    )
f_sore_plot

f_sore_plot = f_sore_all |>
    dplyr::filter(wet_dry == "dry") |>
    ggplot( 
        aes(x = Wavelength, 
            y = Reflectance,
            group = plot,
            color = Northing)) +
    theme_bw() +
    annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
    geom_line(size = 0.65) +
    scale_x_continuous(breaks = seq(300, 800, 200)) +
    scale_color_continuous(type = "viridis") +
    labs(x = "Wavelength (nm)", y = "Reflectance (%)") +
    theme(
        legend.position = "none"
    )
f_sore_plot

ggsave("lfe_objects/reflectance_spectroscopy/f_sore_dry.jpeg", 
       plot = f_sore_plot, device = "jpeg", 
       width = 8, height = 6, units = "in", dpi = 600) 


f_sore_UV_summ = f_sore_all |>
    dplyr::group_by(plot) |>
    dplyr::filter(Wavelength < 400) |>
    dplyr::summarise(mean_UV = mean(Reflectance),
                     mean_canopy = mean(canopy_cover))
f_sore_UV_summ 

f_sore_canopy_cover_UV = f_sore_UV_summ |>
    ggplot(aes(x = mean_canopy,
               y = mean_UV)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_x_continuous(name = "Canopy Cover (%)") +
    scale_y_continuous(name = "F. soredica UV Reflectance (%)") +
    # breaks = c(25, 50, 75, 100, 125)) +
    theme_bw()
f_sore_canopy_cover_UV

f_sore_final = ggarrange(f_sore_plot,f_sore_canopy_cover_UV,
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1)
f_sore_final

ggsave("lfe_objects/reflectance_spectroscopy/f_sore_final.jpeg",
       f_sore_final,
       width = 7,
       height = 3,
       units = "in",
       dpi = 600)
