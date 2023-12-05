##### Script for handling Microclimate Data #####

# call header
source("header.R")

#' Step 1: Convert Hobo file to .csv using HoboWare Software.
#' Step 2: Load into R.


plot31 = read_csv("lfe_data/microclimate/OSMP_19.csv") |>
    dplyr::select(c(1,2,3,4)) |>
    dplyr::rename(number = 1,
                  DT = 2,
                  temp = 3,
                  RH = 4)
plot31 = plot31[c(97:1321), ]

plot10 = read_csv("lfe_data/microclimate/OSMP_32.csv") |>
    dplyr::select(c(1,2,3,4)) |>
    dplyr::rename(number = 1,
                  DT = 2,
                  temp = 3,
                  RH = 4)
plot10 = plot10[c(1:1225),]

plots = rbind(plot10, plot31)
plots$plot = c(rep("P10", 1225), rep("P31", 1225))
plots$num = c(rep(seq(1:1225),2))

plots$VPD = vpd(temp = plots$temp,
                RH = plots$RH)

vpd_plot = ggplot(data = plots,
                   mapping = aes(x = num,
                                 y = VPD,
                                 color = plot)) +
    geom_line(size = 0.7) +
    scale_color_manual(values = c("Black", "Dark Green"),
                       labels = c("Streamside",
                                  "SE-facing Slope"),
                       name = "Plots") +
    geom_abline(slope = 0, intercept = 1) +
    ylab("Vapor Pressure Deficit (kPa)") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())
vpd_plot   

RH_plot = ggplot(data = plots,
                   mapping = aes(x = num,
                                 y = RH,
                                 color = plot)) +
    geom_line(size = 0.7) +
    scale_color_manual(values = c("Black", "Dark Green"),
                       labels = c("Streamside",
                                  "SE-facing Slope"),
                       name = "Plots") +
    geom_abline(slope = 0, intercept = 80) +
    ylab("Relative Humidity (%)") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())
RH_plot 

temp_plot = ggplot(data = plots,
                 mapping = aes(x = num,
                               y = temp,
                               color = plot)) +
    geom_line(size = 0.7) +
    scale_color_manual(values = c("Black", "Dark Green"),
                       labels = c("Streamside",
                                 "SE-facing Slope"),
                       name = "Plots") +
    geom_abline(slope = 0, intercept = 15) +
    ylab(expression("Temperature " ( degree~C))) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())
temp_plot

ggsave("lfe_objects/microclimate/temp.jpeg", plot = temp_plot, device = "jpeg", width = 8, height = 2, units = "in", dpi = 300)   
ggsave("lfe_objects/microclimate/RH.jpeg", plot = RH_plot, device = "jpeg", width = 8, height = 2, units = "in", dpi = 300)   
ggsave("lfe_objects/microclimate/vpd.jpeg", plot = vpd_plot, device = "jpeg", width = 8, height = 2, units = "in", dpi = 300)   

## loading in exported .csvs from HOBOware

# read in all files from the folder
microclimate = ldply(paste("lfe_data/microclimate/Sept_revisits/", 
                           list.files(path = "lfe_data/microclimate/Sept_revisits"), 
                           sep = ""), 
                     read.csv,
                     col.names = (c("Num", "DT", "Temp", "RH")), 
                     header=TRUE)

# extract number of file rows per file
file_rows = c()
for (i in 1:31) {
    file_rows[i] = nrow(read.csv(paste("lfe_data/microclimate/Sept_revisits/", 
                                       list.files(path = "lfe_data/microclimate/Sept_revisits"), 
                                       sep = "")[[i]]))
    }

# extract plot numbers
plot_nums = c()
for (i in 1:31) {
    plot_nums[i] = as.numeric(substr(sub('.*plot_', '', 
                       list.files(path = "lfe_data/microclimate/Sept_revisits")[[i]]),
                   1, 
                   nchar(sub('.*plot_', '', 
                             list.files(path = "lfe_data/microclimate/Sept_revisits")[[i]])) - 4))
}

# add column of plot number
microclimate$plot = rep.int(plot_nums, file_rows)

# convert date character to date and time format using lubridate
microclimate$dt = mdy_hms(microclimate$DT,
                        tz="America/Denver")

# filter to only include times when all dataloggers were deployed
microclimate_red = microclimate |>
    filter(dt > mdy_hms("06-25-2023 00:00:00 AM",
                             tz="America/Denver") &
               dt < mdy_hms("09-15-2023 00:00:00 AM",
           tz="America/Denver"))

# calculate vpd for every row
microclimate_red$vpd = vpd(temp = microclimate_red$Temp,
                           RH = microclimate_red$RH)

# calculate time spent below vpd threshold
microclimate_red$wet_time = ifelse(microclimate_red$vpd < 0.75,
                                     1,
                                     0)

ggplot(data = microclimate_red, aes(x = dt,
                                y = vpd,
                                color = as.character(plot))) +
    geom_line()

# cut off temps, rh, vpds, and averages..
microclimate_summ = microclimate_red |>
    dplyr::group_by(plot) |>
    dplyr::summarise(mean_vpd = mean(vpd),
                     mean_temp = mean(Temp),
                     mean_RH = mean(RH),
                     max_vpd = max(vpd),
                     max_temp = max(Temp),
                     max_RH = max(RH),
                     min_vpd = min(vpd),
                     min_temp = min(Temp),
                     min_RH = min(RH),
                     wet_hours = 0.5 * sum(wet_time))


# save summary table as .csv
write_csv(microclimate_summ, "lfe_objects/microclimate/summer_microclim_summ_table.csv")
write_csv(microclimate_red, "lfe_objects/microclimate/summer_microclimate.csv")
