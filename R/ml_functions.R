data_preparation <- function(raw_dat) {
  # remove the id columns
  dat_cols <- raw_dat %>% select(-starts_with("id.")) %>% 
    # remove extra rB columns
    select( -c(
      starts_with("imp"),
      starts_with("unimp"))) %>% 
    select(-matches("^rB[0-9]{1,2}$")) %>% 
    # remove extra landuse vars
    select(-c(
      "commercial", "education", "parkland",
      "residential", "transport", "water",
      "industrial", "hospital_medical",
      "other", "primary_production"
      
    )) %>% 
    mutate(accessibility_pt = accessibility_pt/10e4,
           acc1 = acc1/10,
           acc2_pred = acc2_pred/10)
  
  dat <- as.data.frame(sapply(dat_cols, as.numeric))
  # An overview of the dataset:
  # print(head(dat))
  paste("Thera are in total ", nrow(dat), " rows in the ", "Sydney",
    " dataset, half of which are censored.",
    sep = ""
  )
  return(dat)
}

convert_dat_to_task <- function(dat) {
  # Building the task
  task.surv <- makeSurvTask(id = "full_task", data = dat, target = c("duration", "event"))
  # task.surv0 = makeSurvTask(id = "empty task", data = dat[,1:3], target = c("duration",  "event"))

  # list the tasks to be used in the benchmark
  list(task.surv)
}

# resampling methods
benchmark_resampling <- function(iters) {
  makeResampleDesc("CV", iters = iters)
}

tuning_resampling <- function(iters) {
  makeResampleDesc("CV", iters = iters)
}

# tuning parameters
tune_fw.perc <- function(upper){
ParamHelpers::makeNumericParam("fw.perc", lower = 0, upper = upper)
}

tune_nfolds <- function(){
  ParamHelpers::makeNumericParam("nfolds", lower = 5, upper = 10)
}

tune_glm_alpha <- function(){
  ParamHelpers::makeNumericParam("alpha", lower = 0.1, upper = 0.9)
}

tune_SRC_nodesize <- function(){
  ParamHelpers::makeIntegerParam("nodesize", lower = 3, upper = 25)
}

tune_mtry <- function(dat){
  ParamHelpers::makeIntegerParam("mtry", lower = floor(sqrt(ncol(dat))), upper = 100)
}

tune_nrounds <- function(){
  ParamHelpers::makeIntegerParam("nrounds", lower = 10, upper = 25)
}

tune_min_node_size <- function(){
  ParamHelpers::makeIntegerParam("min.node.size", lower = 5, upper = 50)
}

tune_max_depth <- function(){
  ParamHelpers::makeIntegerParam("max_depth", lower = 1, upper = 10)
}

tune_xg_eta <- function(){
  ParamHelpers::makeNumericParam("eta", lower = 0.01, upper = 0.4)
}

tune_lambda <- function(){
  ParamHelpers::makeNumericParam("lambda", lower = 0, upper = 50)
}

tune_XG_alpha <- function(){
  ParamHelpers::makeNumericParam("alpha", lower = 0, upper = 50)
}

# tune_ctr <-function(resolution){
#   makeTuneControlGrid(resolution = resolution)
# }

# makeTuneWrapper
TuneWrapper <- function(lrn, inner_resampling, ps, ctrl){
makeTuneWrapper(
  lrn,
  resampling = inner_resampling,
  par.set = ps,
  control = ctrl,
  show.info = F
)
}

# Setting random seed and running the benchmark**
batchmark_run <- function(learners, tasks, resamplings) {
  set.seed(123, "L'Ecuyer")
  try(batchtools::makeExperimentRegistry(
    file.dir = config::get("batchtools_registry"),
    packages = c("survival", "mlr"),
    source = c(
      here::here("R", "RLearner_surv_reg.R"),
      here::here("R", "RLearner_surv_xgboost.R"),
      here::here("R", "RLearner_surv_coxboost.R")
    )
  )) # initialise don't worry if this returns an error
  reg <- batchtools::loadRegistry(config::get("batchtools_registry"), writeable = TRUE)
  reg$cluster.functions <- batchtools::makeClusterFunctionsSocket(1)
  res <- batchmark(learners, tasks, resamplings, models = T, keep.pred = TRUE, keep.extract = T, reg = reg)
  batchtools::submitJobs()
  batchtools::waitForJobs()
  mlr::reduceBatchmarkResults()
}

benchmark_run <- function(learners, tasks, res_instances){
  parallelStartSocket(config::get("parallel_map_workers"), level = "mlr.tuneParams")
  parallelLibrary(packages = c("mlr", "survival", "xgboost"), level = "mlr.tuneParams")
  parallelSource(files = list.files(here::here("R"), full.names = TRUE), level = "mlr.tuneParams")
  bench_res <- benchmark(learners, tasks, res_instances, models = T, keep.pred = T, keep.extract = T)
  parallelStop()
  return(bench_res)
}

benchmark_run_multicore <- function(learners, tasks, res_instances){
  parallelStartMulticore(config::get("parallel_map_workers"), level = "mlr.tuneParams")
  parallelLibrary(packages = c("mlr", "survival", "xgboost"), level = "mlr.tuneParams")
  parallelSource(files = list.files(here::here("R"), full.names = TRUE), level = "mlr.tuneParams")
  bench_res <- benchmark(learners, tasks, res_instances, models = T, keep.pred = T, keep.extract = T)
  parallelStop()
  return(bench_res)
}

benchmark_run_multicore2 <- function(learners, tasks, res_instances){
  parallelStartSocket(config::get("parallel_map_workers"), level = "mlr.selectFeatures")
  parallelLibrary(packages = c("mlr", "survival", "xgboost", "gbm", "randomForestSRC", "ranger"))
  parallelSource(files = list.files(here::here("R"), full.names = TRUE))
  bench_res <- benchmark(learners, tasks, res_instances, models = T, keep.pred = T, keep.extract = T)
  parallelStop()
  return(bench_res)
}

feature_count <- function(bm) {
  varnum <- data.frame(
    learner = character(),
    numvars = double()
  )

  j <- 1
  for (i in getBMRLearnerIds(bm)) {
    if (mean(sapply(sapply(getBMRModels(bm)$full_task[[i]], getFilteredFeatures), length)) == 1) {
      varnum <- rbind(varnum, c(i, length(sapply(getBMRModels(bm)$full_task[[i]], getFilteredFeatures)) / 3))
    } else {
      varnum <- rbind(varnum, c(i, mean(sapply(sapply(getBMRModels(bm)$full_task[[i]], getFilteredFeatures), length))))
    }
    j <- j + 1
  }
  return(varnum)
}


feature_selected <- function(bm) {
  features <- data.frame(
    learner = integer(),
    class = integer(),
    feature = character()
  )

  for (i in getBMRLearnerIds(bm)[-(1:13)]) {
    if (mean(sapply(sapply(getBMRModels(bm)$full_task[[i]], getFilteredFeatures), length)) == 1) {
      for (j in 1:3) {
        features <- rbind(features, cbind("learner" = rep(i, nrow(as.data.frame(sapply(getBMRModels(bm)$full_task[[i]], getFilteredFeatures)[, j]))), "class" = j, "feature" = (sapply(getBMRModels(bm)$full_task[[i]], getFilteredFeatures)[, j])))
      }
    } else {
      for (j in 1:3) {
        features <- rbind(features, cbind("learner" = rep(i, nrow(as.data.frame(sapply(getBMRModels(bm)$full_task[[i]], getFilteredFeatures)[[j]]))), "class" = j, "feature" = (sapply(getBMRModels(bm)$full_task[[i]], getFilteredFeatures)[[j]])))
      }
    }
  }

  return(features)
}

export_benchmark <- function(benchmark) {
  write.csv(benchmark, "Results/benchmark_raw.csv", row.names = FALSE)
  write.csv(getBMRAggrPerformances(benchmark), "Results/benchmark-aggregate_raw.csv", row.names = FALSE)
  write.csv(getBMRPerformances(benchmark, drop = TRUE), "Results/benchmark-simp_raw.csv", row.names = FALSE)
  write.csv(getBMRPerformances(benchmark, as.df = TRUE), "Results/benchmark-df_raw.csv", row.names = FALSE)
  write.csv(getBMRPredictions(benchmark), "Results/benchmark-prediction_raw.csv", row.names = FALSE)
  feature_counts <- feature_count(benchmark)
  write.csv(feature_counts, "Results/benchmark-varnums.csv", row.names = FALSE)
  features_selected <- feature_selected(benchmark)
  write.csv(features_selected, "Results/benchmark-features.csv", row.names = FALSE)
}

split_cv_to_holdouts <- function(holdout_desc, res_instances, task) {
  lapply(seq_len(res_instances$desc$iters), function(.x) {
      holdout_instance <- makeResampleInstance(holdout_desc, task)
      holdout_instance$train.inds <- list(res_instances$train.inds[[.x]])
      holdout_instance$test.inds <- list(res_instances$test.inds[[.x]])
      holdout_instance
  })
}