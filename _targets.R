library(targets)
library(tarchetypes)
library(future)
library(future.batchtools)
plan(batchtools_torque, template = "batchtools.torque.tmpl")

tar_option_set(
  workspace_on_error = TRUE,
  packages = c(
    "magrittr",
    "here", "tidyverse", "tidymodels", "data.table", "rlang", "mlr", "gbm", "lubridate",
    "survival", "parallel", "janitor", "sf", "googleway", "xgboost",
    "parallelMap"
  ),
  memory = "transient",
  storage = "worker",
  retrieval = "worker",
  garbage_collection = TRUE
)

options(
  clustermq.scheduler = ifelse(Sys.getenv("LETS_MULTIPROCESS") == 1, "multiprocess", config::get("clustermq.scheduler")),
  clustermq.template = config::get("clustermq.template"),
  # clustermq.scheduler = "multiprocess",
  java.parameters = config::get("java.parameters")
)

# load functions
for (file in list.files("R", full.names = TRUE)) source(file)

# End this file with a list of target objects.
list(
  # Machine learning model ---------------
  # Import raw data --------------------------
  tar_target(surveys_raw, raw_data_surveys("Sydney")),
  tar_target(people_raw, raw_data_people("Sydney")),
  tar_target(education_raw, raw_data_education("Sydney")),
  tar_target(employment_raw, raw_data_employment("Sydney")),
  tar_target(location_raw, raw_data_location("Sydney")),
  tar_target(vehicle_raw, raw_data_vehicle("Sydney")),
  # find submitted surveys rows
  tar_target(surveys_submitted, submitted_surveys(surveys_raw)),
  tar_target(surveys, submitted_rows(surveys_raw, surveys_submitted)),
  tar_target(people, submitted_rows(people_raw, surveys_submitted)),
  tar_target(education, submitted_rows(education_raw, surveys_submitted)),
  tar_target(employment, submitted_rows(employment_raw, surveys_submitted)),
  tar_target(location, submitted_rows(location_raw, surveys_submitted)),
  tar_target(vehicle, submitted_rows(vehicle_raw, surveys_submitted)),
  # find household members
  tar_target(hh_members, find_household_members(location)),
  # prepare required variables
  tar_target(raw_dat, var_preparation(surveys, people, education, employment, location, vehicle, hh_members, city = "Sydney")),
  tar_target(dat_vars, var_preparation(surveys, people, education, employment, location, vehicle, hh_members, city = "Sydney")),
  tar_target(home_locs, loc_data_home(surveys, location)),
  tar_target(home_edu_emp_locs, loc_data(surveys, home_locs, education, employment)),
  tar_target(all_suburbs_in_survey, extract_all_suburbs_in_survey(home_edu_emp_locs)),
  tar_target(fixed_locations_path, path("Data", "Processed", "Accessibility", "fixed_locations.csv"), format = "file"),
  tar_target(all_suburbs_in_survey_fixed, fix_all_suburbs(all_suburbs_in_survey,
    read_from = fixed_locations_path)),
  # tar_target(geocoded_suburbs_in_survey, geocode_locations(all_suburbs_in_survey_fixed)),
  tar_target(home_edu_emp_locs_geocoded_file, path("Data", "Processed", "Accessibility", "home_edu_emp_locs_geocoded.csv"), format = "file"),
  tar_target(home_locs_geocoded_file, path("Data", "Processed", "Accessibility", "home_locs_geocoded.csv"), format = "file"),
  tar_target(home_edu_emp_locs_geocoded, fread(input = home_edu_emp_locs_geocoded_file)),
  tar_target(home_locs_geocoded, fread(input = home_locs_geocoded_file)),
  # > accessibility data -----------
  tar_target(suburb_landuse_sf_file, path("Data", "Processed", "Accessibility", "suburb_landuse_sf.csv"), format = "file"),
  tar_target(car_ttm_file, path("Data", "Processed", "Accessibility", "car_ttm.csv"), format = "file"),
  tar_target(pt_ttm_file, path("Data", "Processed", "Accessibility", "pt_ttm.csv"), format = "file"),
  tar_target(suburb_landuse_sf, fread(input = suburb_landuse_sf_file)),
  tar_target(car_ttm, fread(input = car_ttm_file)),
  tar_target(pt_ttm, fread(input = pt_ttm_file)),
  # add accessibility data -------------------------
  tar_target(fixed_car_ttm_path, path("Data", "Processed", "Accessibility", "fixed_car_ttm.csv"), format = "file"),
  tar_target(car_ttm_extra, extra_tt_func(car_ttm, fixed_car_ttm_path)),
  tar_target(find_tt, find_tt_func(home_edu_emp_locs_geocoded, car_ttm_extra, pt_ttm)),
  tar_target(dat_tt, add_tt_data(dat_vars, find_tt)),
  tar_target(dat_lu, add_landuse_data(dat_tt, home_locs_geocoded, suburb_landuse_sf)),
  tar_target(dat_acc_1, add_accessibility_data(dat_lu, home_locs_geocoded, car_jobaccess, "car")),
  tar_target(dat_acc_2, add_accessibility_data(dat_acc_1, home_locs_geocoded, pt_jobaccess, "pt")),
  # LM - attitude modelling -----------------------
  tar_target(car_jobaccess_file, path("Data", "Processed", "Accessibility", "car_jobaccess.csv"), format = "file"),
  tar_target(pt_jobaccess_file, path("Data", "Processed", "Accessibility", "pt_jobaccess.csv"), format = "file"),
  tar_target(car_jobaccess, fread(input = car_jobaccess_file)),
  tar_target(pt_jobaccess, fread(input = pt_jobaccess_file)),
  tar_target(delta_a_acc_car, accessibility_factor_acc_func(dat_acc_2, "car")),
  tar_target(lm_acc_car, attitude_lm(dat_acc_2, delta_a_acc_car)),
  tar_target(dat_lm, add_acc2_for_censored(dat_acc_2, lm_acc_car)),
  # data to ML
  tar_target(dat, data_preparation(dat_lm)),
  tar_target(tasks, convert_dat_to_task(dat)),
  # tuning parameters
  # tar_target(par_fw.perc, tune_fw.perc(upper)),
  tar_target(par_nfolds, tune_nfolds()),
  tar_target(par_glm_alpha, tune_glm_alpha()),
  tar_target(par_min_node_size, tune_min_node_size()),
  tar_target(par_SRC_nodesize, tune_SRC_nodesize()),
  tar_target(par_mtry, tune_mtry(dat)),
  tar_target(par_nrounds, tune_nrounds()),
  tar_target(par_max_depth, tune_max_depth()),
  tar_target(par_xg_eta, tune_xg_eta()),
  tar_target(par_lambda, tune_lambda()),
  tar_target(par_XG_alpha, tune_XG_alpha()),
  # learners
  tar_target(learner_file, path("Data", "Raw", "learners", "learners.csv"), format = "file"),
  tar_target(learner_list, read_csv(learner_file)),
  tar_target(learners, combine_learners(learner_list,
                                        inner_resampling,
                                        # par_fw.perc,
                                        par_nfolds,
                                        par_glm_alpha,
                                        par_SRC_nodesize,
                                        par_min_node_size,
                                        par_mtry,
                                        par_nrounds,
                                        par_max_depth,
                                        par_xg_eta,
                                        par_lambda,
                                        par_XG_alpha
  )),
  # run benchmark
  tar_target(resamplings, benchmark_resampling(iters = 5)),
  tar_target(inner_resampling, tuning_resampling(iters = 5)),
  tar_target(res_instances, {set.seed(20220606); makeResampleInstance(resamplings, tasks[[1]])}),
  # tar_target(ctrl, tune_ctr(resolution = 20)),
  tar_target(benchmark, benchmark_run(learners, tasks, res_instances), pattern = map(learners), iteration = "list", error = "continue"),
  # run benchmark on each resample
  tar_target(holdout_desc, makeResampleDesc("Holdout", split = 0.8)),
  tar_target(holdout_instances, split_cv_to_holdouts(holdout_desc, res_instances, tasks[[1]])),
  tar_target(benchmark_resamples, benchmark_run(learners, tasks, holdout_instances), pattern = cross(learners, holdout_instances), iteration = "list")
)

