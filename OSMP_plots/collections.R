##### Collections Analyses #####

# header
source("header.R")

# load in data
colxn = read_csv("lfe_data/OSMP_plots/plot_collections_11_16_23.csv") |>
    rename(plot = Plot)

# load in plot characteristics
plot_chars = read_csv("lfe_objects/OSMP_plots/plot_chars_full.csv")

# merge dfs
df = merge(colxn, plot_chars, by = "plot")

# does rockiness relate to microhabitat availability?
summ_colxn = colxn |>
    group_by(plot) |>
    summarise(mean_height = mean(`Lichen Height`),
              sd_height = sd(`Lichen Height`),
              mean_slope = mean(Slope),
              sd_slope = sd(Slope))

summ_df = merge(summ_colxn, plot_chars, by = "plot")

# correlation
rock_height = summ_df |>
    ggplot(aes(x = Rockiness,
               y = mean_height)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_x_continuous(name = "Rockiness",
                       breaks = c(1:10)) +
    scale_y_continuous(name = "Mean Lichen Height",
                       breaks = c(25, 50, 75, 100, 125)) +
    theme_bw()
rock_height

rock_height_sd = summ_df |>
    ggplot(aes(x = Rockiness,
               y = sd_height)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_x_continuous(name = "Rockiness",
                       breaks = c(1:10)) +
    scale_y_continuous(name = "Lichen Height\nStandard Deviation") +
    theme_bw()
rock_height_sd

rock_slope = summ_df |>
    ggplot(aes(x = Rockiness,
               y = mean_slope)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_x_continuous(name = "Rockiness",
                       breaks = c(1:10)) +
    scale_y_continuous(name = "Mean Lichen Slope")+
                      # breaks = c(25, 50, 75, 100, 125)) +
    theme_bw()

rock_slope

rock_slope_sd = summ_df |>
    ggplot(aes(x = Rockiness,
               y = sd_slope)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_x_continuous(name = "Rockiness",
                       breaks = c(1:10)) +
    scale_y_continuous(name = "Lichen Slope\nStandard Deviation") +
    # breaks = c(25, 50, 75, 100, 125)) +
    theme_bw()

rock_slope_sd


rockiness_plot = ggarrange(rock_height, rock_height_sd, rock_slope, rock_slope_sd, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
rockiness_plot

ggsave("lfe_objects/OSMP_plots/rockiness_plot.jpeg",
       rockiness_plot,
       width = 7,
       height = 5,
       units = "in",
       dpi = 600)

# F. soredica and P. sulcata

p_sulc = df |>
    dplyr::filter(`Specific Epithet` == "sulcata")

f_sore = df |>
    dplyr::filter(`Specific Epithet` == "soredica")

# regress relative abundance by plot vpd
f_sore_vpd_ra = f_sore |>
    ggplot(aes(x = mean_vpd,
               y = `Relative Abundance`)) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(x = "Mean VPD") +
    theme_bw()
f_sore_vpd_ra

# tree type
f_sore_tree_ra = f_sore |>
    ggplot(aes(x = `Dominant Tree Species`,
               y = `Relative Abundance`)) +
    geom_boxplot() +
    theme_bw()
f_sore_tree_ra

# repeat wit p_sulc
p_sulc_vpd_ra = p_sulc |>
    ggplot(aes(x = mean_vpd,
               y = `Relative Abundance`)) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(x = "Mean VPD") +
    theme_bw()
p_sulc_vpd_ra

p_sulc_tree_ra = p_sulc |>
    ggplot(aes(x = `Dominant Tree Species`,
               y = `Relative Abundance`)) +
    geom_boxplot() +
    theme_bw()
p_sulc_tree_ra

sp_plot = ggarrange(f_sore_vpd_ra, f_sore_tree_ra, p_sulc_vpd_ra, p_sulc_tree_ra, 
                           labels = c("A", "B", "C", "D"),
                           ncol = 2, nrow = 2)
sp_plot

ggsave(
       "lfe_objects/OSMP_plots/sp_plot.jpeg",
       sp_plot,
       width = 7,
       height = 5,
       units = "in",
       dpi = 600)
