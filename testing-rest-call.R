library(httr)
library(sf)
library(dplyr)
library(jsonlite)
library(geojsonsf)
library(leaflet)

# load the ids for all objects
id_url <- "https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/1/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryPolygon&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=true&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=json"

response <- GET(id_url)

# Check for a successful response and parse the data
if (http_type(response) == "application/json") {
  # Parse the response to text then use fromJSON to convert read json
  ids <- content(response, "text") %>%
    fromJSON() %>%
    as_tibble()
  # Process the data as needed
} else {
  cat("Unexpected response type:", http_type(response))
}

# define the base url for the layer queries
base_url <- "https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/1/query?"

# Define query parameters using map to iterate over the different object id's
# obtained via the query above
query_params <- purrr::map(ids$objectIds, ~ list(
  # objectIds filters to one HU at a time from the map call
  objectIds = .,      
  # return polygons
  geometryType = "esriGeometryPolygon",
  # return geometries
  returnGeometry = "true",
  # Output format (e.g., geojson, json, etc.)
  f = "geojson" 
))

# create list of full urls to map over
full_urls <- purrr::map(query_params, ~ modify_url(base_url, query = .))

# get a subset of the full urls to load for smaller sample
# full dataset appears to be 55.6 MB in size
full_urls_small <- full_urls[1:15]

# map over the full urls with geojson_sf
wbd_data <- purrr::map(full_urls, geojson_sf)

# reduce to a dataframe where each row is an sf object
wbd_data_df <- wbd_data %>%
  purrr::reduce(bind_rows) %>%
  filter(name != "Caribbean Region" & name != "South Pacific Region") %>%
  # texas has some weird lines going on inside the shape, but this does not
  # remove them like I thought it would
  st_make_valid() %>%
  # simplify shapes to 1 km
  st_simplify(preserveTopology = FALSE, dTolerance = 500)

# plot
leaflet() %>%
  addTiles() %>%
  addPolygons(data=wbd_data_df, weight=1, color="steelblue", opacity = 0.4)

# can do either
# mapview(wbd_data)
# mapview(wbd_data_df)



