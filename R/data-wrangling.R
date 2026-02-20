library(quadkeyr)
library(sf)
library(terra)
library(tigris)

gpkg <- "data/air-quality.gpkg"

# project area -----------------------------------------------------------
slc <- subset(
  places("Utah", cb = TRUE),
  NAME == "Salt Lake City",
  select = NAME
)

slc <- st_transform(slc, 4326)
bb8 <- st_bbox(slc)

write_sf(
  slc,
  dsn = gpkg,
  layer = "slc"
)

# bing map quadkeys ------------------------------------------------------
# to filter to the datasets we need, we have to use bing - bing?! - map quadkeys
ms_buildings <- read.csv(file.path(
  "https://minedbuildings.z5.web.core.windows.net",
  "global-buildings",
  "dataset-links.csv"
))

names(ms_buildings) <- tolower(names(ms_buildings))

quads <- subset(
  ms_buildings,
  location == "UnitedStates",
  select = c(quadkey, url)
)

# the quadkeys are technically level 9 resolution, but stored as integers in csv
# so we have to pad zeroes
quads <- transform(quads, quadkey = sprintf("%09d", quadkey))
quads <- quadkey_df_to_polygon(quads)
quads <- st_filter(quads, slc)

# buildings --------------------------------------------------------------
buildings <- vector("list", nrow(quads))

for (i in seq_along(quads[["hrefs"]])) {
  # run in a local environment to avoid adding temporary variables to the
  # global environment
  buildings[[i]] <- local({
    fn <- tempfile(fileext = ".csv.gz")
    download.file(hrefs[i], fn)

    # read lines
    con <- gzfile(fn, open = "rb")
    lines <- readLines(con)
    close(con)

    # combine into a feature collection text string
    # this makes reading the string into an sf data.frame more efficient
    fc_text <- paste0(
      '{"type":"FeatureCollection","features":[',
      paste(lines, collapse = ","),
      ']}'
    )
    read_sf(fc_text)
  })
}

buildings <- do.call(rbind, buildings)

write_sf(
  buildings,
  dsn = gpkg,
  layer = "buildings"
)

# rasterize --------------------------------------------------------------
r_extent <- ext(buffer(vect(quads), 2500))

# raster template with ~30x30-meter resolution
r_template <- rast(
  crs = "EPSG:4326",
  extent = r_extent,
  resolution = c(0.00027, 0.00027),
  vals = 1
)

r_buildings <- mask(
  r_template,
  vect(buildings),
  inverse = TRUE
)

writeRaster(
  r_buildings,
  filename = "data/buildings-30m.tif"
)
