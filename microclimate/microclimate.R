##### Script for handling Microclimate Data #####

#' Step 1: Convert Hobo file to .csv using HoboWare Software.
#' Step 2: Load into R.

# working directory
setwd("~/R/Microclimate")

library(readr)
plot31 = read_csv("OSMP_19.csv") |>
    dplyr::select(c(1,2,3,4)) |>
    dplyr::rename(number = 1,
                  DT = 2,
                  temp = 3,
                  RH = 4)
plot31 = plot31[c(97:1321), ]

plot10 = read_csv("OSMP_32.csv") |>
    dplyr::select(c(1,2,3,4)) |>
    dplyr::rename(number = 1,
                  DT = 2,
                  temp = 3,
                  RH = 4)
plot10 = plot10[c(1:1225),]

plots = rbind(plot10, plot31)
plots$plot = c(rep("P10", 1225), rep("P31", 1225))
plots$num = c(rep(seq(1:1225),2))

vpd = function(
        temp,
        RH
){
    
    vpd = 0.611 * 2.718^(17.502 * temp/(temp + 240.97)) * (1 - RH/100)
    return(vpd)
}

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

ggsave("temp.jpeg", plot = temp_plot, device = "jpeg", width = 8, height = 2, units = "in", dpi = 300)   
ggsave("RH.jpeg", plot = RH_plot, device = "jpeg", width = 8, height = 2, units = "in", dpi = 300)   
ggsave("vpd.jpeg", plot = vpd_plot, device = "jpeg", width = 8, height = 2, units = "in", dpi = 300)   

    
    