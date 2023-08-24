libs <- c(
  "tidyverse", "sf", "geodata",
  "terra", "classInt", "rayshader"
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

urls <- c(
"https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_S27W060_Map.tif",
"https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_S27W057_Map.tif"
)

for (url in urls) {
    download.file(
        url,
        destfile = basename(url),
        mode = "wb"
    )
}

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

paraguari_sf <- country_borders |>
  dplyr::filter(
    NAME_1 == "Paraguarí"
      )

plot(sf::st_geometry(paraguari_sf)) 

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
        paraguari_sf
      ),
      snap = "in",
      mask = T
    )
  }
)

forest_height_mosaic <- do.call(
    terra::mosaic,
    forest_height_rasters
)

forest_height_paraguari <- forest_height_mosaic |>
terra::aggregate(
    fact = 2
)

forest_height_paraguari_df <- forest_height_paraguari |>
  as.data.frame(
    xy = T
  )

names(forest_height_paraguari_df)[3]<- "height"
head(forest_height_paraguari_df)

breaks <- classInt::classIntervals(
    forest_height_paraguari_df$height,
    n = 5,
    style = "equal"
)$brks

cols <-
    c(
        "white", "#ffd3af", "#fbe06e",
        "#6daa55", "#205544"
    )

texture <- colorRampPalette(
    cols,
    bias = 2
)(6)

p <- ggplot(
    forest_height_paraguari_df
) +
geom_raster(
    aes(
        x = x,
        y = y,
        fill = height
    )
) +
scale_fill_gradientn(
    name = "height (m)",
    colors = texture,
    breaks = round(breaks, 0)
) +
guides(
    fill = guide_legend(
        direction = "vertical",
        keyheight = unit(5, "mm"),
        keywidth = unit(5, "mm"),
        title.position = "top",
        label.position = "right",
        title.hjust = .5,
        label.hjust = .5,
        ncol = 1,
        byrow = F
    )
) +
theme_minimal() +
theme(
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = c(.1, .6),
    legend.title = element_text(
        size = 11, color = "grey10"
    ),
    legend.text = element_text(
        size = 10, color = "grey10"
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    legend.background = element_rect(
        fill = "white", color = NA
    ),
    panel.border = element_blank(),
    plot.margin = unit(
        c(
            t = 0, r = 0,
            b = 0, l = 0
        ), "lines"
    )
)


h <- nrow(forest_height_paraguari)
w <- ncol(forest_height_paraguari)

rayshader::plot_gg(
    ggobj = p,
    width = w / 1000,
    height = h / 1000,
    scale = 150,
    solid = F,
    shadow = T,
    shadowcolor = "white",
    shadowwidth = 0,
    shadow_intensity = 1,
    background = "white",
    offset_edges = F,
    sunangle = 315,
    window.size = c(800, 800),
    zoom = .5,
    phi = 30,
    theta = -30,
    multicore = T
)

rayshader::render_camera(
    phi = 80,
    zoom = .7,
    theta = 5
)

rayshader::render_highquality(
    filename = "paraguari-forest-height-2020.png",
    preview = T,
    interactive = F,
    light = T,
    lightdirection = c(
        315, 320, 315, 320
    ),
    lightintensity = c(
        1500, 1750, 700, 800
    ),
    lightaltitude = c(
        15, 15, 85, 85
    ),
    ground_material = 
    rayrender::microfacet(
        roughness = .6
    ),
    width = 4000,
    height = 4000
)
