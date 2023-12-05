##### meteoland R package #####
# https://emf-creaf.github.io/meteolandbook/
# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://cran.r-project.org/web/packages/meteoland/meteoland.pdf
# https://cran.rstudio.com/web/packages/meteoland/vignettes/tidy-meteoland.html
# https://cran.r-project.org/web/packages/meteoland/vignettes/reshaping-meteo.html

# header
source("header.R")

# read in microclimate data
summer_microclimate = read_csv("lfe_objects/microclimate/summer_microclimate.csv")
str(summer_microclimate)

# solar irradiance based on slope and aspect and day of year


