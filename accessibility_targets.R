library(targets)

tar_option_set(
  packages = c(
    "magrittr", "dplyr", "data.table", "readr", "lubridate",
    # accesibility tools
    "r5r", "sf", "osmextract", "gtfsrouter"
  )
)

options(
  # clustermq.scheduler = "multiprocess",
  java.parameters = config::get("java.parameters")
)

for (file in list.files("R", full.names = TRUE)) source(file)

list(
  # Prepare data for generating suburb skim matrices ----
  tar_target(project_crs, 4326),
  tar_target(dzn_2016_jobs_by_incp_file, get_dzn_2016_jobs_by_incp_file(), format = "file"),
  tar_target(dzn_2016_jobs_by_incp, read_table_builder_csv_string(dzn_2016_jobs_by_incp_file)),
  # tar_target(dzn_jobs, read_table_builder_csv_string(dzn_jobs_file)),
  tar_target(dzn_2016_url, get_dzn_2016_url(), format = "url"),
  tar_target(dzn_2016_sf, load_shp_zip_to_sf(file_url = dzn_2016_url, crs = project_crs)),
  tar_target(dzn_2016_centroids, get_zone_centroids(dzn_2016_sf, zone_col = "dzn_code16")),
  tar_target(dzn_2016_centroids_with_jobs, fuse_jobs(dzn_2016_centroids, dzn_2016_jobs_by_incp)),
  tar_target(suburb_2016_url, get_suburb_2016_url(), format = "url"),
  tar_target(suburb_2016_sf, load_shp_zip_to_sf(file_url = suburb_2016_url, crs = project_crs)),
  tar_target(suburb_2016_centroids, get_zone_centroids(suburb_2016_sf, zone_col = "ssc_name16")),
  # Import raw data --------------------------
  tar_target(surveys_raw, raw_data_surveys("Sydney")),
  tar_target(education_raw, raw_data_education("Sydney")),
  tar_target(employment_raw, raw_data_employment("Sydney")),
  tar_target(location_raw, raw_data_location("Sydney")),
  # find submitted surveys rows
  tar_target(surveys_submitted, submitted_surveys(surveys_raw)),
  tar_target(surveys, submitted_rows(surveys_raw, surveys_submitted)),
  tar_target(education, submitted_rows(education_raw, surveys_submitted)),
  tar_target(employment, submitted_rows(employment_raw, surveys_submitted)),
  tar_target(location, submitted_rows(location_raw, surveys_submitted)),
  # find all locations
  tar_target(home_locs, loc_data_home(surveys, location)),
  tar_target(home_edu_emp_locs, loc_data(surveys, home_locs, education, employment)),
  tar_target(all_suburbs_in_survey, extract_all_suburbs_in_survey(home_edu_emp_locs)),
  tar_target(fixed_locations_path, path("Data", "Processed", "Accessibility", "fixed_locations.csv"), format = "file"),
  tar_target(all_suburbs_in_survey_fixed, fix_all_suburbs(all_suburbs_in_survey,
    read_from = fixed_locations_path
  )),
  tar_target(geocoded_suburbs_in_survey, geocode_locations(all_suburbs_in_survey_fixed)),
  tar_target(home_edu_emp_locs_geocoded, home_edu_emp_locs_geocoded_func(home_edu_emp_locs, geocoded_suburbs_in_survey, fixed_locations_path)),
  tar_target(home_locs_geocoded, home_locs_geocoded_func(home_locs, geocoded_suburbs_in_survey)),
  # land-use targets
  tar_target(mb_2016_url, get_mb_2016_url(), format = "url"),
  tar_target(mb_2016_sf, load_shp_zip_to_sf(file_url = mb_2016_url, crs = project_crs)),
  tar_target(equivalent_ssc_path, path("Data", "Processed", "Accessibility", "equivalient_ssc_name16.csv"), format = "file"),
  tar_target(geocoded_suburbs_in_survey_lu, fix_geocoded_suburbs(geocoded_suburbs_in_survey,
                                                                 read_from = equivalent_ssc_path
  )),
  tar_target(landuse_sf, compute_suburb_landuse(mb_2016_sf, suburb_2016_sf, geocoded_suburbs_in_survey_lu)),
  tar_target(suburb_landuse_sf, aggregate_suburb_landuse(landuse_sf)),
  
  tar_target(survey_centroids, extract_centroids_from_geocodes(geocoded_suburbs_in_survey)),
  tar_target(osm_data_url, get_osm_data_url(), format = "url"),
  tar_target(osm_data_pbf_path, download_osm_data(osm_data_url, osm_path), format = "file"),
  tar_target(osm_path, path("Data", "Raw", "r5r", "australia-190101-internal.osm.pbf")),
  tar_target(gtfs_path, download_gtfs(url = "https://transitfeeds.com/p/transport-for-nsw/237/20190318/download"), format = "file"),
  tar_target(gtfs, extract_gtfs(gtfs_path)),
  tar_target(
    car_jobaccess,
    generate_accessibility(
      osm_data_pbf_path,
      gtfs_path,
      origins = suburb_2016_centroids,
      destinations = dzn_2016_centroids_with_jobs,
      mode = c("CAR"),
      opportunities_colname = "jobs",
      max_trip_duration = 30,
      max_walk_dist = 1000,
      cutoffs = 30L
    )
  ),
  tar_target(
    pt_jobaccess,
    generate_accessibility(
      osm_data_pbf_path,
      gtfs_path,
      origins = suburb_2016_centroids,
      destinations = dzn_2016_centroids_with_jobs,
      mode = c("TRANSIT", "WALK"),
      opportunities_colname = "jobs",
      max_trip_duration = 30,
      max_walk_dist = 1000,
      cutoffs = 30L
    )
  ),
  tar_target(
    car_ttm,
    generate_ttm(
      osm_data_pbf_path,
      gtfs_path,
      origins = survey_centroids,
      destinations = survey_centroids,
      mode = c("CAR")
    )
  ),
  tar_target(
    pt_ttm,
    generate_ttm(
      osm_data_pbf_path,
      gtfs_path,
      origins = survey_centroids,
      destinations = survey_centroids,
      mode = c("TRANSIT", "WALK")
    )
  ),
  tar_target(
    export_car_ttm,
    export_to_csv(
      car_ttm,
      save_to = path("Data", "Processed", "Accessibility", "car_ttm.csv")
    )
  ),
  tar_target(
    export_pt_ttm,
    export_to_csv(
      pt_ttm,
      save_to = path("Data", "Processed", "Accessibility", "pt_ttm.csv")
    )
  ),
  tar_target(
    export_car_jobaccess,
    export_to_csv(
      car_jobaccess,
      save_to = path("Data", "Processed", "Accessibility", "car_jobaccess.csv")
    )
  ),
  tar_target(
    export_pt_jobaccess,
    export_to_csv(
      pt_jobaccess,
      save_to = path("Data", "Processed", "Accessibility", "pt_jobaccess.csv")
    )
  ),
  tar_target(
    export_suburb_landuse_sf,
    export_to_csv(
      suburb_landuse_sf,
      save_to = path("Data", "Processed", "Accessibility", "suburb_landuse_sf.csv")
    )
  ),
  tar_target(
    export_home_edu_emp_locs_geocoded,
    export_to_csv(
      home_edu_emp_locs_geocoded,
      save_to = path("Data", "Processed", "Accessibility", "home_edu_emp_locs_geocoded.csv")
    )
  ),
  tar_target(
    export_home_locs_geocoded,
    export_to_csv(
      home_locs_geocoded,
      save_to = path("Data", "Processed", "Accessibility", "home_locs_geocoded.csv")
    )
  )
)
