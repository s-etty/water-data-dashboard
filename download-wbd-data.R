##################
#
# define functions for loading the HUC layer data
#
##################

# download the huc ids of a particular HUC level, i.e., 2 digit, 4 digit, etc.
get_huc_ids_df <- function(layer = 1) {
  id_url <- glue::glue("https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/{layer}/query?where=1%3D1&returnIdsOnly=true&f=json")

  response <- httr::GET(id_url)
  
  # Check for a successful response and parse the data
  if (httr::http_type(response) == "application/json") {
    # Parse the response to text then use fromJSON to convert read json
    ids <- httr::content(response, "text") |>
      jsonlite::fromJSON() |>
      tibble::as_tibble()
    # Process the data as needed
  } else {
    cat("Unexpected response type:", httr::http_type(response))
  }
  return(ids)
}

# build the urls for the specified layer
build_huc_layer_urls <- function(huc_ids_df, layer = 1) {
  base_url <- glue::glue("https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/{layer}/query?")
  
  # Define query parameters using map to iterate over the different object id's
  # obtained via the query above
  query_params <- purrr::map(huc_ids_df$objectIds, ~ list(
    # objectIds filters to one HU at a time from the map call
    objectIds = .,      
    # return polygons
    geometryType = "esriGeometryPolygon",
    # return geometries
    returnGeometry = "true",
    # Output format (e.g., geojson, json, etc.)
    f = "geojson" 
  ))
  
  # create list of full urls
  full_urls <- purrr::map(query_params, ~ httr::modify_url(base_url, query = .))
  return(full_urls)
}

# read each layer, transform, make valid, then simplify
# the purpose of this is shrink them as they come in rather than all at once
# this end up being slower, but it will use less memory
download_huc_layers <- function(huc_layer_url, keep = 0.1) {
  # surpress warning about incomplete final line
  dat <- sf::st_read(huc_layer_url) 
    # # convert to wgs 84
    # sf::st_transform(crs = '+proj=longlat +datum=WGS84') |>
    # # make any incorrect shapes valid
    # sf::st_make_valid() |>
    # # simplify shapes to specified tolerance (in meters)
    # rmapshaper::ms_simplify(keep = keep)
  return(dat)
}

# wrapper for the above functions
# layer specifies which level to get, tolerance applies a simplifying function
# to make it a smaller dataframe. See st_simplify for more info.
get_huc_layers <- function(layer = 1, keep = 0.1) {
  huc_urls <- get_huc_ids_df(layer = layer) |>
    build_huc_layer_urls(layer = layer)
  
  wbd_layers <- purrr::map(huc_urls, ~ download_huc_layers(., keep = keep))
  
  wbd_layers_df <- wbd_layers |>
    purrr::reduce(dplyr::bind_rows) |>
    # convert to wgs 84
    sf::st_transform(crs = '+proj=longlat +datum=WGS84') |>
    # make any incorrect shapes valid
    sf::st_make_valid()
  
  return(wbd_layers_df)
}

# 55.5 mb without simplifying at HUC 02 level
wbd_data <- get_huc_layers()

wbd_data <- wbd_data |>
  rmapshaper::ms_simplify(keep = 0.1)
