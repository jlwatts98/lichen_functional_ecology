##### Plot Characteristics #####

# call header
source("header.R")

# read in plot characteristic csv
plot_chars = read_csv("lfe_data/OSMP_plots/plot_characteristics.csv") |>
    dplyr::rename(plot = Plot)

# read in microclimate summary
plot_micro = read_csv("lfe_objects/microclimate/summer_microclim_summ_table.csv")

# read in canopy cover csv
plot_canopy = read_csv("lfe_data/OSMP_plots/canopy_cover.csv")

# merge everything into one dataframe
plots = merge(plot_chars, plot_micro, by = "plot")
plots = merge(plots, plot_canopy, by = "plot")

# reduce and save
plots_red = plots[,c(1,3,4,7,5,6,17,18,19,21,22,20,31)] |>
    dplyr::rename(`Mean Summer Temp` = mean_temp,
                  `Mean Summer Relative Humidity` = mean_RH,
                  `Mean Summer Vapor Pressure Deficit` = mean_vpd,
                  `Canopy Cover` = canopy_cover,
                  Plot = plot,
                  `Elevation (Ft.)` = Elevation)
plots_red$`Mean Summer Temp` = signif(plots_red$`Mean Summer Temp`, 3)
plots_red$`Mean Summer Relative Humidity` = signif(plots_red$`Mean Summer Relative Humidity`, 3)
plots_red$`Mean Summer Vapor Pressure Deficit` = signif(plots_red$`Mean Summer Vapor Pressure Deficit`, 3)
plots_red$`Canopy Cover` = signif(plots_red$`Canopy Cover`, 3)


write_csv(plots_red, "lfe_objects/OSMP_plots/plot_chars.csv")

# first break aspect in Northing and Easting
plots$Northing = as.numeric(lapply(plots$Aspect, northing))
plots$Easting = as.numeric(lapply(plots$Aspect, easting))

write_csv(plots, "lfe_objects/OSMP_plots/plot_chars_full.csv")
# correlations


str(plots)

corr_data = plots[, c(5,32,33, 7,8, 9, 20,21,22,31)]
chart.Correlation(corr_data, histogram=TRUE, pch=19)

corr_data2 = plots[, c(7,32,33,31, 20,21,22)] |>
    dplyr::rename(`Mean VPD` = mean_vpd,
                  `Mean Temp` = mean_temp,
                  `Mean RH` = mean_RH,
                  `Canopy Cover` = canopy_cover)



# correlation matrix
jpeg("lfe_objects/OSMP_plots/corr_plot.jpeg", quality = 100, width = 7.5,
     height = 5, units = "in", res = 600)
pairs.panels(corr_data2, 
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
             pch = 19
)

dev.off()

chart.Correlation(corr_data2, histogram=TRUE, pch=19)

