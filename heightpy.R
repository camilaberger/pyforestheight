libs <- c(
  "tidyverse", "sf", "geodata",
  "terra", "classInt", "rayshader", "raster"
)

#install.packages('terra', repos='https://rspatial.r-universe.dev')

installed_libs <- libs %in% rownames(
  installed.packages()
)

if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

invisible(lapply(
  libs,
  library,
  character.only = T
))

#raster_files <- raster("C:/R/height_paraguay/heightpy/ETH_GlobalCanopyHeight_10m_2020_S27W057_Map.tif")


raster_files <-
  list.files(
    path = getwd(),
    pattern = "ETH",
    full.names = T)

get_country_borders <- function(){
  main_path <- getwd()
  country_borders <- geodata::gadm(
    country = "PRY",
    level = 1,
    path = main_path
  ) |>
    sf::st_as_sf()
  return(country_borders)}

country_borders <- get_country_borders()
unique(
  country_borders$NAME_1)

paraguay_sf <- country_borders |>
  dplyr::filter(
    !NAME_1 %in% c(
      "San Pedro", "Alto Paraguay", "Alto Paraná", "Amambay", "Asunción", "Boquerón", "Caaguazú", "Caazapá", "Canindeyú", "Canindeyú", "Concepción", "Cordillera", "Guairá", "Itapúa", "Misiones","Ñeembucú", "Presidente Hayes")
      ) |>
  sf::st_union()

plot(sf::st_geometry(paraguay_sf)) 

forest_height_list <- lapply(
  raster_files,
  terra::rast
)

forest_height_rasters <- lapply(
  forest_height_list,
  function(x) {
    terra::crop(
      x,
      terra::vect(
        paraguay_sf
      ),
      snap = "in",
      mask = T
    )
  }
)

#forest_height_mosaic <- do.call(
#terra::mosaic,
#forest_height_rasters)

#forest_height_paraguay <- forest_height_rasters |>
  #terra::aggregate(
    #fact = 10)

#To vizualize raster file in ggplot we need to convert to data frame.
#data frame holds information about the centroid of every pixel from your raster

# 4. RASTER TO DATAFRAME
#-----------------------

forest_height_paraguay_df <- forest_height_rasters|>
  as.data.frame(
    xy = T
  )
head(forest_height_paraguay_df)[3]<- "height"
