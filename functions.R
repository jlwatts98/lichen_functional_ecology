##### Orginial functions for Lichen Functional Ecology #####

vpd = function(
        temp,
        RH
){
    
    vpd = 0.611 * 2.718^(17.502 * temp/(temp + 240.97)) * (1 - RH/100)
    return(vpd)
}

northing = function(
        aspect
){
    if (is.na(aspect)) {
        return(NA)
    }
    if (aspect > 180) {
        northing = aspect - 270
    } else
    {
        northing = 90 - aspect
    } 
    return(northing)
}

easting = function(
        aspect
){
    if (aspect < 180) {
        easting = 90 - abs(90 - aspect)
    } else
    {
        easting = (90 - abs(270 - aspect)) * -1
    }
    return(easting)
}

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
#timestamps <- c(
#    as.POSIXct("2023-11-15 06:00:00"),
#    as.POSIXct("2023-11-16 12:00:00"),  # Different date
#    as.POSIXct("2023-11-15 18:00:00"),
#    as.POSIXct("2023-11-15 22:00:00")
#)

#sunrise_times <- c(
#    as.POSIXct("2023-11-15 05:45:00"),
#    as.POSIXct("2023-11-16 05:30:00"),  # Different date
#    as.POSIXct("2023-11-15 05:15:00"),
#    as.POSIXct("2023-11-15 05:00:00")
#)

#sunset_times <- c(
#    as.POSIXct("2023-11-15 18:15:00"),
#    as.POSIXct("2023-11-16 18:30:00"),  # Different date
#    as.POSIXct("2023-11-15 18:45:00"),
#    as.POSIXct("2023-11-15 19:00:00")
#)

# Create a vector indicating whether it's daytime or nighttime
#daytime_indicator <- sapply(1:length(timestamps), function(i) {
#    is_daytime(timestamps[i], sunrise_times, sunset_times)
#})

# Print the result
#print(daytime_indicator)

# add in a sunrise/sunset component
# https://psl.noaa.gov/boulder/boulder.sunset.html


#Takes as an input a stars raster layer
#CJ Brown 2020-05-18

st_as_raster <- function(rstars){
    rext <- st_bbox(rstars)
    raster(t(rstars[[1]]), xmn = rext[1], xmx = rext[3],
           ymn = rext[2], ymx=rext[4],
           crs = st_crs(rstars)$proj4string)
}
