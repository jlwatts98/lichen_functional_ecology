##### meteoland example #####

points_to_interpolate_example
raster_to_interpolate_example
meteoland_meteo_example
names(meteoland_meteo_example)

# creating the interpolator object
interpolator <- with_meteo(meteoland_meteo_example) |>
    create_meteo_interpolator()

# performing the interpolation
points_interpolated <- points_to_interpolate_example |>
    interpolate_data(interpolator)

points_interpolated

meteo_without_temp <- meteoland_meteo_example
meteo_without_temp[["MinTemperature"]] <- NULL
meteo_without_temp[["MaxTemperature"]] <- NULL
with_meteo(meteo_without_temp)

# parameters
get_interpolation_params(interpolator)

# interpolated meteo for the first location
points_interpolated[["interpolated_data"]][1]

tidyr::unnest(points_interpolated, cols = "interpolated_data")

class(interpolator)
interpolator

get_interpolation_params(interpolator)
# wind_height parameter
get_interpolation_params(interpolator)$wind_height
#> [1] 10

# set a new wind_height parameter and check
interpolator <- set_interpolation_params(interpolator, params = list(wind_height = 5))
#> ℹ Some interpolation parameters are missing, using default values for those
get_interpolation_params(interpolator)$wind_height

temporal_folder <- tempdir()
write_interpolator(interpolator, file.path(temporal_folder, "interpolator.nc"))
#> ℹ Creating nc file following the NetCDF-CF conventions <https://cfconventions.org/cf-conventions/cf-conventions.html>
#> ℹ Adding spatial info to nc file
#> ✔ Done
# file should exists now
file.exists(file.path(temporal_folder, "interpolator.nc"))
#> [1] TRUE
#> 
file_interpolator <- read_interpolator(file.path(temporal_folder, "interpolator.nc"))
# the read interpolator should be identical to the one we have already
identical(file_interpolator, interpolator)

# min temperature N and alpha before calibration
get_interpolation_params(interpolator)$N_MinTemperature
#> [1] 30
get_interpolation_params(interpolator)$alpha_MinTemperature
#> [1] 3

# calibration
interpolator <- interpolator_calibration(
    interpolator,
    variable = "MinTemperature",
    N_seq = c(5, 20),
    alpha_seq = c(1, 10),
    update_interpolation_params = TRUE
)
#> ℹ Total number of stations: 189
#> ℹ Number of stations with available data: 185
#> ℹ Number of stations used for MAE calculation: 185
#> ℹ Number of parameters combinations to test: 4
#> ℹ Starting evaluation of parameter combinations for "MinTemperature"...
#> • Evaluating N: 5, alpha: 1...
#> • Evaluating N: 5, alpha: 10...
#> • Evaluating N: 20, alpha: 1...
#> • Evaluating N: 20, alpha: 10...
#> ✔ Calibration done: Minimum MAE: 1.18956628899488; N: 20; alpha: 10

# parameters after calibration
get_interpolation_params(interpolator)$N_MinTemperature
#> [1] 20
get_interpolation_params(interpolator)$alpha_MinTemperature
#> [1] 10
#> 
#> 
interpolator <- with_meteo(meteoland_meteo_example) |>
    create_meteo_interpolator() |>
    interpolator_calibration(
        variable = "MinTemperature",
        N_seq = c(5, 20),
        alpha_seq = c(1, 10),
        update_interpolation_params = TRUE
    ) |>
    interpolator_calibration(
        variable = "MaxTemperature",
        N_seq = c(5, 20),
        alpha_seq = c(1, 10),
        update_interpolation_params = TRUE
    ) |>
    interpolator_calibration(
        variable = "DewTemperature",
        N_seq = c(5, 20),
        alpha_seq = c(1, 10),
        update_interpolation_params = TRUE
    ) |>
    write_interpolator(
        filename = file.path(temporal_folder, "interpolator.nc"),
        .overwrite = TRUE
    )

cross_validation <- interpolation_cross_validation(interpolator, verbose = FALSE)
cross_validation$errors

cross_validation$station_stats
cross_validation$r2

summarise_interpolated_data(
    points_interpolated,
    fun = "mean",
    frequency = "week"
)

points_interpolated <- points_to_interpolate_example |>
    interpolate_data(interpolator) |>
    summarise_interpolated_data(
        fun = "mean",
        frequency = "week"
    ) |>
    summarise_interpolated_data(
        fun = "max",
        frequency = "month"
    ) |>
    mutate(
        monthly_erosivity = precipitation_rainfall_erosivity(
            interpolated_data,
            longitude = sf::st_coordinates(geometry)[,1],
            scale = 'month'
        )
    )

points_interpolated
raster_to_interpolate_example

raster_interpolated <- raster_to_interpolate_example |>
    interpolate_data(interpolator)
raster_interpolated

summarise_interpolated_data(
    raster_interpolated,
    fun = "mean",
    frequency = "week"
)

monthly_mean_temperature <- raster_to_interpolate_example |>
    interpolate_data(interpolator, variables = "Temperature") |>
    summarise_interpolated_data(
        fun = "max",
        frequency = "month",
        variable = "MeanTemperature"
    )
plot(monthly_mean_temperature)

# go through reshaping data for meteoland package
# build interpolator for my area
# figure out subday values!

##### Reshaping data for meteoland compatibility #####
meteoland_meteo_example
names(meteoland_meteo_example)

unformatted_meteo
