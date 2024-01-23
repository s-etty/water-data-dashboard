library(dataRetrieval)
library(dplyr)
library(sf)
library(lubridate)
library(leaflet)


# this code is for daily stream values - this means the site reports a daily
# stream flow value rather than just instantaneous. If it doesn't report daily
# values it's less reliable or something. Probably won't have stats either
p_code <- "00060"

# commented out so not running long queries
# # map over state abbreviations and get all sites that are active and reporting
# # daily stream flow values
# site_list <- purrr::map(state.abb, ~ whatNWISdata(stateCd = .,
#                                                   parameterCd = p_code,
#                                                   siteStatus = "active"))
# # turn into single data frame
# # 26,110 active sites
# # 9,512 active sites reporting daily value up to last 90 days
# site_df <- site_list |>
#   purrr::reduce(bind_rows) |>
#   filter(data_type_cd == "dv" & end_date >= today() - days(90))

site_df <- readr::read_csv("Water-Quality-App/data/dv-stream-sites.csv")
site_df_head <- slice_sample(site_df, n = 500)

# get data for
site_dv_data <- readNWISdv(siteNumbers = site_df_head$site_no,
                        parameterCd = p_code,
                        startDate = today() - days(90),
                        endDate = today()) |>
  renameNWISColumns()

# get most recent daily value for sites
site_dv_data <- site_dv_data |>
  filter(!is.na(Flow)) |>
  group_by(site_no) |>
  arrange(desc(Date)) |>
  slice_head(n = 1) |>
  ungroup() |>
  # extract the date info for joining below
  mutate(meas_day = day(Date),
         meas_month = month(Date))

# list out the desired stat vals. p10, etc. are 10th percentile, etc.
stat_vals <- c("mean", "median", "min", "max", "p10", "p25", "p75", "p90")
# calculate the number of groups to break the query into for the function below
n_groups <- ceiling(nrow(site_df_head) / 10)
  
# this query is limited to 10 sites at a time
site_dv_stats <- site_df_head %>%
  # %/% is integer division
  mutate(group = rep(1:n_groups, each = 10)) %>%
  group_split(group) %>%
  purrr::map(., ~ readNWISstat(
    siteNumbers = .$site_no,
    parameterCd = p_code,
    statReportType = "daily",
    statType = stat_vals
  )) %>%
  purrr::reduce(bind_rows) %>%
  filter(end_yr >= year(today() - years(1))) %>%
  distinct()

# join the rest of the info back to the daily measures
site_dv_data_full <- site_dv_data |>
  left_join(site_df_head, by = "site_no") |>
  left_join(site_dv_stats, by = c("site_no",
                                  "meas_day" = "day_nu",
                                  "meas_month" = "month_nu"))

site_dv_data_full <- site_dv_data_full |>
  mutate(case_when(
    Flow <= min_va ~ tibble(status_text = "Lowest Recorded Value for this Date",
                            status_color = "#67001f",
                            status_val = -3),
    Flow > min_va & Flow <= p10_va ~ tibble(status_text = "<10th Percentile",
                                            status_color = "#b2182b",
                                            status_val = -2),
    Flow > p10_va & Flow <= p25_va ~ tibble(status_text = "10th - 25th Percentile",
                                            status_color = "darkorange",
                                            status_val = -1),
    Flow > p25_va & Flow <= p75_va ~ tibble(status_text = "25th - 75th Percentile",
                                            status_color = "forestgreen",
                                            status_val = 0),
    Flow > p75_va & Flow <= p90_va ~ tibble(status_text = "75th - 90th Percentile",
                                            status_color = "lightblue",
                                            status_val = 1),
    Flow > p90_va & Flow < max_va ~ tibble(status_text = ">90th Percentile",
                                           status_color = "steelblue",
                                           status_val = 2),
    Flow >= max_va ~ tibble(status_text = "Highest Recorded Value for this Date",
                            status_color = "darkblue",
                            status_val = 3),
    .default = tibble(status_text = "No Stats Available for this Point on this Date",
                      color = "grey",
                      status_val = NA_real_)
  ))

site_dv_data_full_sf <- site_dv_data_full %>%
  st_as_sf(coords = c("dec_long_va", "dec_lat_va")) |>
  st_set_crs("+proj=longlat +datum=WGS84")

wbd_data <- st_read(dsn = "Water-Quality-App/data/wbd-huc-02/wbd-huc-02.shp") |>
  st_transform(crs = "+proj=longlat +datum=WGS84")

wbd_data_site_meas <- wbd_data |>
  st_join(site_dv_data_full_sf, st_contains, left = TRUE)

wbd_data_site_meas <- wbd_data_site_meas |>
  as_tibble() |>
  group_by(name) |>
  summarise(status_sum_val = sum(status_val, na.rm = TRUE),
            num_sites = n(),
            status_avg = status_sum_val / num_sites) |>
  ungroup()

# wbd_data_site_meas <- wbd_data_site_meas |>
#   mutate(huc_color = case_when(
#     status_avg <= -2 ~ "#67001f",
#     status_avg > -2 & status_avg < -1 ~ "darkorange",
#     status_avg >= -1 & status_avg <= 1 ~ "forestgreen",
#     status_avg > 1 & status_avg <= 2 ~ "lightblue",
#     status_avg > 2 ~ "darkblue",
#     .default = NA_character_
#   ))

wbd_data_summ <- wbd_data |>
  left_join(wbd_data_site_meas, by = "name")

pal <- colorNumeric(c("darkred", "forestgreen", "darkblue"), -3:3)
  
# # Calculate centroid
# label_centroid <- st_centroid(wbd_data_summ)
# 
# # Extract coordinates from centroid
# centroid_coords <- st_coordinates(centroid)

map <-leaflet(data = site_dv_data_full_sf) %>%
  addTiles() %>%
  # addCircles(color = ~ status_color,
  #            fillColor = ~ status_color,
  #            popup = ~paste("<b>Status:</b> ", status_text,
  #                           "<br><i>Flow:</i> ", Flow,
  #                           "<br><i>Mean:</i> ", mean_va)) %>%
  addPolygons(data = wbd_data_summ,
              fillColor = ~ pal(status_avg),
              fillOpacity = 0.4,
              color = "grey",
              weight = 1,
              popup = ~paste("<b>Hydrologic Unit Name:</b> ", name,
                             "<br><i>Number of Sites Measured:</i> ", num_sites,
                             "<br><i>Average Status Value:</i> ", status_avg
                             )
  )

map

# Real-time discharge at a site
instFlow <- readNWISdata(sites = "05114000",
                         service = "iv", 
                         parameterCd = "00060", 
                         startDate = "2014-05-01T00:00Z",
                         endDate = "2014-05-01T12:00Z",
                         tz = "America/Chicago") 
