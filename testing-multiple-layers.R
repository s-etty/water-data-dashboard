library(sf)
library(purrr)
library(dplyr)
library(stringr)
library(mapview)
library(leaflet)

# load list of layer names
wbd_layers <- st_layers("Water-Quality-App/data/WBD_01_HU2_GDB.gdb")
# get the layers that start with WBDHU
wbd_layers <- wbd_layers$name[str_starts(wbd_layers$name, "WBDHU")]
# sort by number and create vector of layer names where 2 is first
wbd_layers <- data.frame(layer_name = wbd_layers) |>
  mutate(layer_number = as.numeric(str_extract(layer_name, "[:digit:]+"))) |>
  arrange(layer_number) %>%
  mutate(layer_zoom_start = case_when(
    layer_number == 2 ~ 1,
    layer_number == 4 ~ 7,
    layer_number == 6 ~ 9,
    layer_number == 8 ~ 11,
    layer_number == 10 ~ 13,
    layer_number == 12 ~ 15,
    .default = NA_real_
  )) %>%
  mutate(layer_zoom_end = case_when(
    layer_number == 2 ~ 6,
    layer_number == 4 ~ 8,
    layer_number == 6 ~ 10,
    layer_number == 8 ~ 12,
    layer_number == 10 ~ 14,
    layer_number == 12 ~ 17,
    .default = NA_real_
  ))

wbd_layers_small <- wbd_layers |>
  slice_head(n = 2)

wbd_data_layers <- wbd_layers_small %>%
  mutate(layer_data = map(.$layer_name,
                          ~ st_read(dsn = "Water-Quality-App/data/WBD_01_HU2_GDB.gdb",
                                    layer = .))) %>%
  mutate(layer_data = map(.$layer_data,
      ~st_transform(., crs = '+proj=longlat +datum=WGS84')))

# wbd_data_layers <- map(wbd_layers_small$layer_name,
#                        ~ st_read(dsn = "Water-Quality-App/data/WBD_01_HU2_GDB.gdb",
#                                  layer = .)) %>%
#   map(., ~st_transform(., crs = '+proj=longlat +datum=WGS84'))

# wbd_data <- st_read("Water-Quality-App/data/WBD_01_HU2_GDB.gdb",
#                               layer = "WBDHU8") |>
#   st_transform(crs = '+proj=longlat +datum=WGS84')

# mapview(wbd_data_layers$layer_data)

m <- leaflet() %>%
  addTiles()

for (i in 1:nrow(wbd_data_layers)) {
  layer <- wbd_data_layers[i,]
  m <- m %>%
    addPolygons(data = layer$layer_data[[1]],
                fillColor = "steelblue",
                fillOpacity = 0.2,
                color = "black",
                weight = 1,
                group = layer$layer_name) %>%
    groupOptions(layer$layer_name, 
                 zoomLevels = layer$layer_zoom_start:layer$layer_zoom_end)
}
# display the map
m

m <- m %>%
  map(wbd_data_layers$layer_data, 
      addPolygons)

  addPolygons(data = wbd_data_layers[[1]],
              fillColor = "steelblue",
              fillOpacity = 0.2, 
              color = "black",
              weight = 1, group = "HU2") %>%
  addPolygons(data = wbd_data_layers[[2]],
              fillColor = "steelblue",
              fillOpacity = 0.2, 
              color = "black",
              weight = 1, group = "HU4") %>%
  groupOptions("HU2", zoomLevels = 1:6) %>%
  groupOptions("HU4", zoomLevels = 7:20)

# Display the map.
map
