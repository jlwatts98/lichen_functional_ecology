##### Analyses #####

# read in processes dataframe
ft_plot = read.csv("lfe_objects/functional_traits/ft_plot.csv")
str(ft_plot)

ft_plot_half = ft_plot |>
    dplyr::filter(state == "Dry")

# Chlorophyll

CH_VPD = ggplot(data = ft_plot_half,
                mapping = aes(x = mean_vpd, y = CH, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(title = "VPD Drives Chlorophyll Concentration per Area",
         x = "VPD", y = "Chlorophyll per Area") +
    geom_smooth(method = "lm")
CH_VPD

CH_CC = ggplot(data = ft_plot_half,
                mapping = aes(x = canopy_cover, y = CH, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(x = "Canopy Cover (%)", y = "Chlorophyll Concentration") +
    geom_smooth(method = "lm") +
    theme(legend.position = "none")
CH_CC

CH_env = ggarrange(CH_VPD, CH_CC,
                   labels = c("A", "B"),
                   ncol = 2, nrow = 1)
CH_env

ggsave("lfe_objects/functional_traits/CH_env.jpeg", plot = CH_env,
       dpi = 600, width = 10, height = 5, units = "in")

# adjust for weight
CH_weight_VPD = ggplot(data = ft_plot_half,
                mapping = aes(x = mean_vpd, y = CH / Dry_Weight, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(title = "VPD does not drive Chlorophyll Concentration per Weight",
         x = "VPD", y = "Chlorophyll per Weight") +
    geom_smooth(method = "lm")
CH_weight_VPD

ggsave("lfe_objects/functional_traits/CH_weight_VPD.jpeg", plot = CH_weight_VPD,
       dpi = 600, width = 7, height = 5, units = "in")

## Chlorophyll vs. algal layer thickness
CH_Algal = ggplot(data = ft_plot_half,
                  mapping = aes(x = Algal, y = CH, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
         x = "Algal Layer Thickness", y = "Chlorophyll per Area") +
    geom_smooth(method = "lm")
CH_Algal

## Self_shading metric CH / Algal vs. VPD
SS_CC = ggplot(data = ft_plot_half,
                mapping = aes(x = canopy_cover, y = CH / Algal, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
         x = "Canopy Cover", y = "Self-shading: Chlorophyll / Algal Layer Thickness") +
    geom_smooth(method = "lm")
SS_CC

SS_env = ggarrange(CH_Algal, SS_CC,
                   labels = c("A", "B"),
                   ncol = 2, nrow = 1)
SS_env

ggsave("lfe_objects/functional_traits/SS_env.jpeg", plot = SS_env,
       dpi = 600, width = 10, height = 5, units = "in")

## STM
STM_VPD = ggplot(data = ft_plot_half,
                       mapping = aes(x = mean_vpd, y = STM, color = species)) +
    geom_point() +
    theme_minimal() +
    scale_colour_discrete(name="Species", 
                          labels=c("Flavopunctelia soredica",
                                "Parmelia sulcata")) +
    labs(
         x = "Vapor Pressure Deficit", y = "Specific Thallus Mass") +
    geom_smooth(method = "lm") +
    theme(legend.text = element_text(size = 8,
                                     face = "italic"))
STM_VPD


ggsave("lfe_objects/functional_traits/STM_VPD.jpeg", plot = STM_VPD,
       dpi = 600, width = 7, height = 5, units = "in")


## WHC
WHC_VPD = ggplot(data = ft_plot_half,
                 mapping = aes(x = mean_vpd, y = WHC, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(title = "VPD does not Drive WHC",
         x = "VPD", y = "Water Holding Capacity") +
    geom_smooth(method = "lm")
WHC_VPD

WHC_CC = ggplot(data = ft_plot_half,
                 mapping = aes(x = canopy_cover, y = WHC, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Canopy Cover May Drive WHC",
         x = "Canopy Cover", y = "Water Holding Capacity") +
    geom_smooth(method = "lm")
WHC_CC

WHC_env = ggarrange(WHC_VPD, WHC_CC,
                   labels = c("A", "B"),
                   ncol = 2, nrow = 1)
WHC_env


ggsave("lfe_objects/functional_traits/WHC_env.jpeg", plot = WHC_env,
       dpi = 600, width = 10, height = 5, units = "in")

MED_WHC = ggplot(data = ft_plot_half,
                 mapping = aes(x = Medulla, y = WHC, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "Medulla Thickness", y = "WHC") +
    geom_smooth(method = "lm")
MED_WHC

## Secondary Metabolites

SM_CHSM = ggplot(data = ft_plot_half,
                 mapping = aes(x = UV_abs, y = CH_UV_abs, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Secondary Metabolites are Correlated",
         x = "Acetone Washed Secondary Metabolites", y = "Acetone Washed and DMSO Incubated SM") +
    geom_smooth(method = "lm")
SM_CHSM

## UV_reflectance correlate with SM?

SM_UV_refl = ggplot(data = ft_plot_half,
                    mapping = aes(x = mean_UV_abs, y = UV_ref, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Secondary Metabolites do not drive UV Reflectance",
         x = "Secondary Metabolites", y = "UV Reflectance") +
    geom_smooth(method = "lm")
SM_UV_refl

## SM vs. canopy_cover and VPD
SM_CC = ggplot(data = ft_plot_half,
               mapping = aes(x = canopy_cover, y = mean_UV_abs, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
         x = "Canopy Cover (%)", y = "UV Absorbance") +
    geom_smooth(method = "lm") +
    theme(legend.position = "none")
SM_CC

SM_VPD = ggplot(data = ft_plot_half,
               mapping = aes(x = mean_vpd, y = mean_UV_abs, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "VPD", y = "UV Absorbance") +
    geom_smooth(method = "lm")
SM_VPD

SM_env = ggarrange(SM_VPD, SM_CC,
                   labels = c("A", "B"),
                   ncol = 2, nrow = 1)
SM_env

ggsave("lfe_objects/functional_traits/SM_env.jpeg", plot = SM_env,
       dpi = 600, width = 10, height = 5, units = "in")

## Thallus layers vs. VPD

UC_VPD = ggplot(data = ft_plot_half,
                mapping = aes(x = mean_vpd, y = Upper.Cortex, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
         x = "VPD", y = "Upper Cortex Thickness") +
    geom_smooth(method = "lm")
UC_VPD

Algal_VPD = ggplot(data = ft_plot_half,
                   mapping = aes(x = mean_vpd, y = Algal, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
         x = "VPD", y = "Algal Layer Thickness") +
    geom_smooth(method = "lm")
Algal_VPD    

MED_VPD = ggplot(data = ft_plot_half,
                 mapping = aes(x = mean_vpd, y = Medulla, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "VPD", y = "Medulla Thickness") +
    geom_smooth(method = "lm")
MED_VPD  

LC_VPD = ggplot(data = ft_plot_half,
                 mapping = aes(x = mean_vpd, y = Lower.Cortex, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "VPD", y = "Lower Cortex Thickness") +
    geom_smooth(method = "lm")
LC_VPD    


TL_VPD = ggarrange(UC_VPD, Algal_VPD, MED_VPD, LC_VPD,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


ggsave("lfe_objects/functional_traits/TL_VPD.jpeg", plot = TL_VPD,
       dpi = 600, width = 7, height = 5, units = "in")

## Canopy Cover vs. Thallus layers

UC_CC = ggplot(data = ft_plot_half,
                mapping = aes(x = canopy_cover, y = Upper.Cortex, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "CC", y = "Upper Cortex Thickness") +
    geom_smooth(method = "lm")
UC_CC

Algal_CC = ggplot(data = ft_plot_half,
                   mapping = aes(x = canopy_cover, y = Algal, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "CC", y = "Algal Layer Thickness") +
    geom_smooth(method = "lm")
Algal_CC    

MED_CC = ggplot(data = ft_plot_half,
                 mapping = aes(x = canopy_cover, y = Medulla, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "CC", y = "Medulla Thickness") +
    geom_smooth(method = "lm")
MED_CC  

LC_CC = ggplot(data = ft_plot_half,
                mapping = aes(x = canopy_cover, y = Lower.Cortex, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "CC", y = "Lower Cortex Thickness") +
    geom_smooth(method = "lm")
LC_CC    


TL_CC = ggarrange(UC_CC, Algal_CC, MED_CC, LC_CC,
                   labels = c("A", "B", "C", "D"),
                   ncol = 2, nrow = 2)
TL_CC

ggsave("lfe_objects/functional_traits/TL_CC.jpeg", plot = TL_CC,
       dpi = 600, width = 7, height = 5, units = "in")

TT_VPD = ggplot(data = ft_plot_half,
               mapping = aes(x = mean_vpd, y = thallus_thickness, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "VPD", y = "Thallus Thickness") +
    geom_smooth(method = "lm")
TT_VPD

TT_CC = ggplot(data = ft_plot_half,
                mapping = aes(x = canopy_cover, y = thallus_thickness, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "CC", y = "Thallus Thickness") +
    geom_smooth(method = "lm")
TT_CC

TT_env = ggarrange(TT_VPD, TT_CC,
                   labels = c("A", "B"),
                   ncol = 2, nrow = 1)
TT_env

ggsave("lfe_objects/functional_traits/TT_env.jpeg", plot = TT_env,
       dpi = 600, width = 10, height = 5, units = "in")


## positions and VPD
north_VPD = ggplot(data = ft_plot_half,
                 mapping = aes(x = mean_vpd, y = coll_northing, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "VPD", y = "Collection Northing") +
    geom_smooth(method = "lm")
north_VPD

height_VPD = ggplot(data = ft_plot_half,
                   mapping = aes(x = mean_vpd, y = Lichen.Height, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "VPD", y = "Collection Height") +
    geom_smooth(method = "lm")
height_VPD


slope_VPD = ggplot(data = ft_plot_half,
                    mapping = aes(x = mean_vpd, y = Slope.y, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "VPD", y = "Collection Slope") +
    geom_smooth(method = "lm")
slope_VPD

coll_env = ggarrange(north_VPD, slope_VPD,
                   labels = c("A", "B"),
                   ncol = 2, nrow = 1)
coll_env

ggsave("lfe_objects/functional_traits/coll_env.jpeg", plot = coll_env,
       dpi = 600, width = 10, height = 5, units = "in")

## spectral indices
PRI_wd = ggplot(ft_plot,
                mapping = aes(x = state,
                              y = PRI,
                              color = species)) +
    geom_boxplot() +
    theme_minimal()
PRI_wd

LI_wd = ggplot(ft_plot,
                mapping = aes(x = state,
                              y = LI,
                              color = species)) +
    geom_boxplot() +
    theme_minimal()
LI_wd

NPCI_wd = ggplot(ft_plot,
                 mapping = aes(x = state,
                               y = NPCI,
                               color = species)) +
    geom_boxplot() +
    theme_minimal()
NPCI_wd

MCARI1_wd = ggplot(ft_plot,
                   mapping = aes(x = state,
                                 y = MCARI1,
                                 color = species)) +
    geom_boxplot() +
    theme_minimal()
MCARI1_wd

SI_wd = ggarrange(PRI_wd, LI_wd, NPCI_wd, MCARI1_wd,
                  labels = c("A", "B", "C", "D"),
                  ncol = 2, nrow = 2)
SI_wd

ggsave("lfe_objects/functional_traits/SI_wd.jpeg", plot = SI_wd,
       dpi = 600, width = 7, height = 5, units = "in")

##### Best Figure #####
# SM_CC
# STM_VPD
# CH_CC

best_figure = ggarrange(SM_CC, CH_CC, STM_VPD,
          labels = c("A", "B", "C"),
          widths = c(1,1,2),
          ncol = 3, nrow = 1)
best_figure

ggsave("lfe_objects/functional_traits/best_figure.jpeg", plot = best_figure,
       dpi = 600, width = 8, height = 3, units = "in")





