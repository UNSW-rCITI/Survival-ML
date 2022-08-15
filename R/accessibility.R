get_osm_data_url <- function() {
  "https://download.geofabrik.de/australia-oceania/australia-190101.osm.pbf"
}

get_mb_2016_url <- function() {
  "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_mb_2016_nsw_shape.zip&1270.0.55.001&Data%20Cubes&E9FA17AFA7EB9FEBCA257FED0013A5F5&0&July%202016&12.07.2016&Latest"
}

get_dzn_2016_url <- function() {
  "https://www.abs.gov.au/statistics/people/population/census-population-and-housing-destination-zones/aug-2016/80000_dzn_2016_aust_shape.zip"
}

get_suburb_2016_url <- function() {
  "https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_ssc_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&5698C77C925DC4FACA25802C001439C5&0&July%202016&13.09.2016&Previous"
}

get_dzn_2016_jobs_by_occp_file <- function() {
  path("Data", "Raw", "ABSTableBuilder", "2016_POW_NSW_DZN_OCCP1.csv")
}

get_dzn_2016_jobs_by_incp_file <- function() {
  path("Data", "Raw", "ABSTableBuilder", "2016_POW_NSW_DZN_INCP1.csv")
}

#' Download and save the file in a temp directory
#'
#' @return save_to path
download_file <- function(url, save_to = NULL, timeout = 300) {
  if (is.null(save_to)) {
    save_to <- tempfile()
  }
  default_timeout <- getOption("timeout")
  options(timeout = timeout)
  download.file(
    url,
    save_to,
    mode = ifelse(Sys.info()["sysname"] == "Windows", "wb", "w")
  )
  message("Saved the downloaded file to: ", save_to)
  options(timeout = default_timeout)
  save_to
}


download_osm_data <- function(osm_data_url, osm_path) {
  download_file(url = osm_data_url, save_to = osm_path)
}

download_gtfs <- function(url) {
  save_to <- path("Data", "Raw", "r5r", paste0("GTFS", ".zip"))
  download_file(url, save_to = save_to)
}

unzip_file <- function(file_to_read) {
  temp_dir <- fs::path(tempdir(), basename(tempfile()))
  message("Extracting the file to: ", temp_dir)
  zip_info <- unzip(file_to_read, exdir = temp_dir)
  temp_dir
}

load_shp_zip_to_sf <- function(x, file_url, crs) {
  if (!missing(file_url)) {
    x <- download_file(file_url) %>%
      unzip_file()
  }
  shp_file <- list.files(x, pattern = ".shp$", full.names = TRUE)
  if (length(shp_file) != 1L) {
    stop("Either no or more than one .shp files were found in the given path.")
  }
  shp <- sf::st_read(shp_file) %>%
    janitor::clean_names() %>%
    sf::st_transform(crs = crs)
}

vectortranslate_osm_pbf_layer <- function(osm_path) {
  lapply(
    c("lines", "points", "multilinestrings", "multipolygons", "other_relations"),
    \(x) {
      osmextract::oe_vectortranslate(
        osm_path,
        layer = x
      )
    }
  )[[1]]
}

read_osm_pbf_layer <- function(osm_path, layer, ...) {
  osmextract::oe_read(osm_path, layer = layer)
}

get_zone_centroids <- function(data_sf, zone_col) {
  data_sf %>%
    dplyr::filter(ste_name16 == "New South Wales") %>%
    sf::st_centroid() %>%
    {
      cbind(
        select(., .data[[zone_col]]) %>% sf::st_drop_geometry(),
        sf::st_coordinates(.)
      )
    } %>%
    dplyr::filter(!is.nan(X)) %>%
    rename(id = zone_col, lon = "X", lat = "Y") %>%
    mutate(id = gsub(" \\(.*", "", id))
}

#' @param departure time as POSIXct (i.e., as.POSIXct("19-01-2019 12:00:00", format = "%d-%m-%Y %H:%M:%S"))
#' @param max_trip_duration duration in minutes
#' @param max_walk_dist distance in metres
generate_ttm <- function(osm_data_pbf_path,
                         gtfs_path, origins,
                         destinations,
                         mode,
                         departure_datetime = as.POSIXct("19-01-2019 08:00:00", format = "%d-%m-%Y %H:%M:%S"),
                         max_walk_dist = 3000,
                         max_trip_duration = 60 * 10) {
  r5r_data_path <- dirname(osm_data_pbf_path)
  r5r_core <- r5r::setup_r5(data_path = r5r_data_path, verbose = FALSE)

  r5r::travel_time_matrix(
    r5r_core = r5r_core,
    origins = origins,
    destinations = destinations,
    mode = mode,
    departure_datetime = departure_datetime,
    max_walk_dist = max_walk_dist,
    max_trip_duration = max_trip_duration,
    n_threads = config::get("r5r_n_threads")
  )
}


#' @param departure time as POSIXct (i.e., as.POSIXct("19-01-2019 12:00:00", format = "%d-%m-%Y %H:%M:%S"))
#' @param max_trip_duration duration in minutes
#' @param max_walk_dist distance in metres
generate_accessibility <- function(osm_data_pbf_path,
                                   gtfs_path, origins,
                                   destinations,
                                   mode,
                                   opportunities_colname,
                                   departure_datetime = as.POSIXct("19-01-2019 08:00:00", format = "%d-%m-%Y %H:%M:%S"),
                                   max_walk_dist = 3000,
                                   max_trip_duration = 60 * 10,
                                   cutoffs = 30L) {
  r5r_data_path <- dirname(osm_data_pbf_path)
  r5r_core <- r5r::setup_r5(data_path = r5r_data_path, verbose = FALSE)

  r5r::accessibility(
    r5r_core = r5r_core,
    origins = origins,
    destinations = destinations,
    opportunities_colname = opportunities_colname,
    mode = mode,
    departure_datetime = departure_datetime,
    max_walk_dist = max_walk_dist,
    max_trip_duration = max_trip_duration,
    n_threads = config::get("r5r_n_threads"),
    cutoffs = cutoffs
  )
}

extract_all_suburbs_in_survey <- function(home_edu_emp_locs) {
  home_edu_emp_locs %>%
    select(ends_with(".loc")) %>%
    unlist() %>%
    unique() %>%
    as.character() %>%
    unique() %>%
    .[!is.na(.)] %>%
    .[. != "Notv"]
}

compute_suburb_landuse <- function(mb_2016_sf, suburb_2016_sf, geocoded_suburbs_in_survey) {
  geocods <- geocoded_suburbs_in_survey %>%
    mutate(formatted_address_ = gsub("[()]", "", formatted_address)) %>%
    select(formatted_address_)


  survey_suburbs_sf <- suburb_2016_sf %>%
    mutate(ssc_name16_ = gsub("[()]", "", ssc_name16)) %>%
    group_by(ssc_code16) %>%
    filter(any(grepl(tolower(ssc_name16_), tolower(geocods$formatted_address_)))) %>%
    select(ssc_name16)


  subset_mb_sf <- mb_2016_sf %>%
    select(mb_code16, mb_cat16) %>%
    st_filter(survey_suburbs_sf)

  st_intersection(subset_mb_sf, survey_suburbs_sf)
}

aggregate_suburb_landuse <- function(landuse_sf) {
  landuse_sf %>%
    mutate(area = sf::st_area(.)) %>%
    st_drop_geometry() %>%
    group_by(ssc_name16, mb_cat16) %>%
    summarise(area = sum(area, na.rm = TRUE)) %>%
    tidyr::pivot_wider(ssc_name16, names_from = "mb_cat16", values_from = "area", values_fill = units::set_units(0, m^2)) %>%
    janitor::clean_names() %>%
    rowwise() %>%
    mutate(total_area = sum(c_across(-any_of("ssc_name16")), na.rm = TRUE)) %>%
    mutate(across(-starts_with(c("ssc", "total")), ~ . / total_area, .names = "{col}_per")) %>%
    ungroup()
}

fuse_jobs <- function(dzn_2016_centroids, dzn_2016_jobs_by_incp) {
  out <- dzn_2016_jobs_by_incp %>%
    select(-any_of("counting")) %>%
    tidyr::pivot_wider(names_from = incp_total_personal_income_weekly, values_from = count) %>%
    rename(id = "dzn_pow") %>%
    mutate(jobs = rowSums(across(-all_of("id")))) %>%
    merge(dzn_2016_centroids, by = "id")
  stopifnot(nrow(out) == nrow(dzn_2016_centroids))
  out %>% select(id, lon, lat, jobs)
}

export_to_csv <- function(x, save_to) {
  stopifnot(dir.exists(dirname(save_to)))
  data.table::fwrite(x, file = save_to)
  "Exported"
}

export_to_rds <- function(x, save_to) {
  stopifnot(dir.exists(dirname(save_to)))
  saveRDS(x, file = save_to)
  "Exported"
}
