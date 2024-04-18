##### Analyses #####

# read in processes dataframe
ft_plot = read.csv("lfe_objects/functional_traits/ft_plot.csv")
str(ft_plot)

ft_plot_half = ft_plot |>
    dplyr::filter(state == "Dry")

# Chlorophyll

CH_VPD = ggplot(data = ft_plot_half,
                mapping = aes(x = mean_vpd, y = CH_area, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(title = "VPD Drives Chlorophyll Concentration per Area",
         x = "VPD", y = "Chlorophyll per Area") +
    geom_smooth(method = "lm")
CH_VPD

CH_CC = ggplot(data = ft_plot_half,
                mapping = aes(x = canopy_cover, y = CH_area, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(x = "Canopy Cover (%)", y = "Chlorophyll Concentration") +
    geom_smooth(method = "lm")
CH_CC

CH_north = ggplot(data = ft_plot_half,
                mapping = aes(x = Northing, y = CH_area, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(x = "Northing", y = "Chlorophyll Concentration") +
    geom_smooth(method = "lm")
CH_north

CH_env = ggarrange(CH_VPD, CH_CC,
                   labels = c("A", "B"),
                   ncol = 2, nrow = 1)
CH_env

ggsave("lfe_objects/functional_traits/CH_env.jpeg", plot = CH_env,
       dpi = 600, width = 10, height = 5, units = "in")

# adjust for weight
CH_weight_VPD = ggplot(data = ft_plot_half,
                mapping = aes(x = mean_vpd, y = CH_weight, color = species)) +
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
                  mapping = aes(x = Algal, y = CH_area, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
         x = "Algal Layer Thickness", y = "Chlorophyll per Area") +
    geom_smooth(method = "lm")
CH_Algal

## Self_shading metric CH / Algal vs. VPD
SS_CC = ggplot(data = ft_plot_half,
                mapping = aes(x = canopy_cover, y = CH_area / Algal, color = species)) +
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
    scale_colour_discrete(name="Species") +
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
                    mapping = aes(x = mean_SM_area, y = UV_ref, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Secondary Metabolites do not drive UV Reflectance",
         x = "Secondary Metabolites", y = "UV Reflectance") +
    geom_smooth(method = "lm")
SM_UV_refl

## SM vs. canopy_cover and VPD
SM_CC = ggplot(data = ft_plot_half,
               mapping = aes(x = canopy_cover, y = mean_SM_area, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
         x = "Canopy Cover (%)", y = "UV Absorbance") +
    geom_smooth(method = "lm") +
    theme(legend.position = "right")
SM_CC

SM_VPD = ggplot(data = ft_plot_half,
               mapping = aes(x = mean_vpd, y = mean_SM_area, color = species)) +
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
                mapping = aes(x = species,
                              y = PRI,
                              color = state)) +
    geom_boxplot() +
    theme_minimal()
PRI_wd

LI_wd = ggplot(ft_plot,
                mapping = aes(x = species,
                              y = LI,
                              color = state)) +
    geom_boxplot() +
    theme_minimal()
LI_wd

NPCI_wd = ggplot(ft_plot,
                 mapping = aes(x = species,
                               y = NPCI,
                               color = state)) +
    geom_boxplot() +
    theme_minimal()
NPCI_wd

MCARI1_wd = ggplot(ft_plot,
                   mapping = aes(x = species,
                                 y = MCARI1,
                                 color = state)) +
    geom_boxplot() +
    theme_minimal()
MCARI1_wd

SI_wd = ggarrange(PRI_wd, LI_wd, NPCI_wd, MCARI1_wd,
                  labels = c("A", "B", "C", "D"),
                  ncol = 2, nrow = 2)
SI_wd

ggsave("lfe_objects/functional_traits/SI_wd.jpeg", plot = SI_wd,
       dpi = 600, width = 7, height = 5, units = "in")

# collection data and WHC
WHC_north = ggplot(data = ft_plot_half,
                   mapping = aes(x = coll_northing, y = WHC, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "Collection Northing", y = "WHC") +
    geom_smooth(method = "lm")
WHC_north

WHC_slope = ggplot(data = ft_plot_half,
                   mapping = aes(x = Slope.y, y = WHC, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "Collection Slope", y = "WHC") +
    geom_smooth(method = "lm")
WHC_slope

# collection data and SM
SM_north = ggplot(data = ft_plot_half,
                   mapping = aes(x = coll_northing, y = SM_area, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "Collection Northing", y = "Secondary Metabolites") +
    geom_smooth(method = "lm")
SM_north

SM_slope = ggplot(data = ft_plot_half,
                   mapping = aes(x = Slope.y, y = SM_area, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "Collection Slope", y = "Secondary Metabolites") +
    geom_smooth(method = "lm")
SM_slope

# pca

# clean up data to only include traits
traits_only = ft_plot_half |>
    dplyr::select(species, sample, 
                  WHC,
                  thallus_thickness, 
                  WL,
                  Lower.Cortex)
pca_matrix = as.matrix(traits_only[3:6])

# do a pca
trait_pca = prcomp(x = pca_matrix)

autoplot(trait_pca, data = ft_plot_half, colour = 'mean_vpd',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)
autoplot(trait_pca, x = 2, y = 3,data = ft_plot_half, colour = 'mean_vpd',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

plot(trait_pca)
pcs = as.data.frame(trait_pca$x)
pcs$sample = traits_only$sample

ft_pc_plot = merge(pcs, ft_plot_half, by = "sample")
ft_pc_plot

ggplot(data = ft_pc_plot,
       mapping = aes(x = PC1,
                     y = PC2,
                     color = species)) +
    geom_point()


# trait correlations
cor = pairs.panels(traits_only[3:8], 
             method = "pearson", # correlation method
             hist.col = "grey",
             density = T,  # show density plots
             ellipses = F, # show correlation ellipses
             scale = F,
             cex = 2,
             cex.cor=1,
             stars = T,
             gap = 0,
             smoother = T,
             cex.axis = 1,
             pch = 19)

cor1 = pairs.panels(ft_plot_half |>
                        dplyr::select( 
                                      CH_area,
                                      SM_area,
                                      WHC, STM, 
                                      thallus_thickness,
                                      WL,
                                      mean_RH, mean_vpd), 
                   method = "pearson", # correlation method
                   hist.col = "grey",
                   density = T,  # show density plots
                   ellipses = F, # show correlation ellipses
                   scale = F,
                   cex = 2,
                   cex.cor=1,
                   stars = T,
                   gap = 0,
                   smoother = T,
                   cex.axis = 1,
                   pch = 19)

cor2 = pairs.panels(ft_plot_half |>
                        dplyr::select(
                            STM,
                            WHC,
                            WL,
                            Upper.Cortex,
                            Algal,
                            Medulla,
                            Lower.Cortex), 
                    method = "pearson", # correlation method
                    hist.col = "grey",
                    density = T,  # show density plots
                    ellipses = F, # show correlation ellipses
                    scale = F,
                    cex = 2,
                    cex.cor=1,
                    stars = T,
                    gap = 0,
                    smoother = T,
                    cex.axis = 1,
                    pch = 19)


# CH and STM
CH_STM = ggplot(data = ft_plot_half,
                mapping = aes(x = STM, y = CH_area, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "Specific Thallus Mass", y = "Chlorophyll") +
    geom_smooth(method = "lm")
CH_STM

# traits and RH
WHC_RH = ggplot(data = ft_plot_half,
                mapping = aes(x = mean_RH, y = WHC, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "Mean RH", y = "WHC") +
    geom_smooth(method = "lm")
WHC_RH
       
TT_RH = ggplot(data = ft_plot_half,
               mapping = aes(x = mean_RH, y = thallus_thickness, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "Mean RH", y = "Thallus Thickness") +
    geom_smooth(method = "lm")     
TT_RH       
       

CH_RH = ggplot(data = ft_plot_half,
               mapping = aes(x = mean_RH, y = CH_area, color = species)) +
    geom_point() +
    theme_minimal() +
    labs(
        x = "Mean RH", y = "Chlorophyll") +
    geom_smooth(method = "lm")     
CH_RH 

       

##### Linear Models #####

TT_lm = lm(data = ft_plot,
           formula = thallus_thickness ~ species +
               mean_vpd)
summary(TT_lm)

WL_lm = lm(data = ft_plot,
           formula = WL ~ species + mean_RH)
summary(WL_lm)

##### Community Weighted Trait Means #####



CWTM = function(
        trait,
        df,
        weights
){
    df$weight = ifelse(df$Relative.Abundance == 1, weights[1],
                ifelse(df$Relative.Abundance == 2, weights[2],
                ifelse(df$Relative.Abundance == 3, weights[3],
                ifelse(df$Relative.Abundance == 4, weights[4],
                ifelse(df$Relative.Abundance == 5, weights[5],
                       NA)))))
    df$toi = df[[trait]]
                       
    
    CWTM = df |>
        dplyr::select(plot.x, sample, toi, weight) |>
        dplyr::group_by(plot.x) |>
        dplyr::summarize(CWTM = weighted.mean(toi, weight)) |>
        dplyr::ungroup() |>
        dplyr::rename(plot = plot.x)
    # rename column
    CWTM[[trait]] = CWTM$CWTM
    CWTM = CWTM[, c(1,3)]
    
    return(CWTM)
}

plot_mean_WHC = CWTM("WHC", ft_plot_half, weights = c(.1, .25, .5, 1, 2))
plot_mean_CH = CWTM("CH_area", ft_plot_half, weights = c(.1, .25, .5, 1, 2))
plot_mean_STM = CWTM("STM", ft_plot_half, weights = c(.1, .25, .5, 1, 2))
plot_mean_TT = CWTM("thallus_thickness", ft_plot_half, weights = c(.1, .25, .5, 1, 2))
plot_mean_SM = CWTM("SM_area", ft_plot_half, weights = c(.1, .25, .5, 1, 2))
plot_mean_WL = CWTM("WL", ft_plot_half, weights = c(.1, .25, .5, 1, 2))

#put all data frames into list
df_list = list(plot_mean_WHC,
               plot_mean_CH,
               plot_mean_STM,
               plot_mean_TT,
               plot_mean_SM,
               plot_mean_WL)

#merge all data frames in list
CWTMs = df_list |> reduce(full_join, by='plot')

# merge with plot characteristics 
plot_chars = read.csv("lfe_objects/OSMP_plots/plot_chars_full.csv")
plot_CWTMs = merge(plot_chars, CWTMs, by = "plot")

cor3 = pairs.panels(plot_CWTMs |>
                        dplyr::select(
                            mean_vpd,
                            mean_RH,
                            Northing,
                            WL,
                            SM_area,
                            CH_area,
                            WHC,
                            STM,
                            ), 
                    method = "pearson", # correlation method
                    hist.col = "grey",
                    density = T,  # show density plots
                    ellipses = F, # show correlation ellipses
                    scale = F,
                    cex = 2,
                    cex.cor=1,
                    stars = T,
                    gap = 0,
                    smoother = T,
                    cex.axis = 1,
                    pch = 19)
    