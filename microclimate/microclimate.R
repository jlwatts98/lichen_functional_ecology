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

# Function to determine if a given timestamp is daytime or nighttime
is_daytime <- function(timestamp, sunrise_times, sunset_times) {
    timestamp_date <- as.Date(timestamp)
    
    # Find the index of the corresponding date in the sunrise and sunset vectors
    date_index <- which(as.Date(sunrise_times) == timestamp_date)
    
    # Check if the date is present in both sunrise and sunset vectors
    if (length(date_index) > 0 && timestamp_date == as.Date(sunset_times[date_index])) {
        timestamp_hour <- as.numeric(format(timestamp, "%H"))
        timestamp_minute <- as.numeric(format(timestamp, "%M"))
        
        sunrise_hour <- as.numeric(format(sunrise_times[date_index], "%H"))
        sunrise_minute <- as.numeric(format(sunrise_times[date_index], "%M"))
        
        sunset_hour <- as.numeric(format(sunset_times[date_index], "%H"))
        sunset_minute <- as.numeric(format(sunset_times[date_index], "%M"))
        
        # Convert times to total minutes for easier comparison
        timestamp_total_minutes <- timestamp_hour * 60 + timestamp_minute
        sunrise_total_minutes <- sunrise_hour * 60 + sunrise_minute
        sunset_total_minutes <- sunset_hour * 60 + sunset_minute
        
        # Check if the timestamp is between sunrise and sunset
        return(timestamp_total_minutes >= sunrise_total_minutes &&
                   timestamp_total_minutes <= sunset_total_minutes)
    } else {
        # If the date is not present in both vectors, return NA (or handle accordingly)
        return(NA)
    }
}

# Example usage:
# Replace the following vectors with your actual data
timestamps <- c(
    as.POSIXct("2023-11-15 06:00:00"),
    as.POSIXct("2023-11-16 12:00:00"),  # Different date
    as.POSIXct("2023-11-15 18:00:00"),
    as.POSIXct("2023-11-15 22:00:00")
)

sunrise_times <- c(
    as.POSIXct("2023-11-15 05:45:00"),
    as.POSIXct("2023-11-16 05:30:00"),  # Different date
    as.POSIXct("2023-11-15 05:15:00"),
    as.POSIXct("2023-11-15 05:00:00")
)

sunset_times <- c(
    as.POSIXct("2023-11-15 18:15:00"),
    as.POSIXct("2023-11-16 18:30:00"),  # Different date
    as.POSIXct("2023-11-15 18:45:00"),
    as.POSIXct("2023-11-15 19:00:00")
)

# Create a vector indicating whether it's daytime or nighttime
daytime_indicator <- sapply(1:length(timestamps), function(i) {
    is_daytime(timestamps[i], sunrise_times, sunset_times)
})

# Print the result
print(daytime_indicator)

# add in a sunrise/sunset component
# https://psl.noaa.gov/boulder/boulder.sunset.html

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
