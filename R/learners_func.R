createLearner <- function(learner.name,
                          learner.id,
                          featSel.name,
                          learner_hyper_fixed,
                          learner_hyper_tune,
                          featSel_hyper_fixed,
                          featSel_hyper_tune_in,
                          fw.perc_upper,
                          featSel_hyper_tune_out,
                          FeatSelFilter,
                          FeatSelWrapper,
                          Tuning,
                          resolution,
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
                          par_XG_alpha) {
  #  Only learner
  if (!FeatSelFilter & !FeatSelWrapper & !Tuning) {
    if (is.na(learner_hyper_fixed)) {
      makeLearner(learner.name, id = learner.id)
    } else {
      makeLearner(learner.name,
                  id = learner.id,
                  par.vals = eval(parse(
                    text = paste0("list(", learner_hyper_fixed, ")")
                  )))
    }
    
    # learner + Filter
  } else if (FeatSelFilter & !FeatSelWrapper & !Tuning) {
    makeFilterWrapper(
      learner = makeLearner(learner.name, id = learner.id),
      fw.method = featSel.name,
      fw.abs = 0
    )
    
    # learner + Tuning
  } else if (!FeatSelFilter & !FeatSelWrapper & Tuning) {
    ps = eval(parse(text = paste0(
      "makeParamSet(", learner_hyper_tune, ")"
    )))
    if (is.na(learner_hyper_fixed)) {
      lrn = makeLearner(learner.name,
                        id = learner.id
      )
      } else{
        lrn = makeLearner(learner.name,
                          id = learner.id,
                          par.vals = eval(parse(
                            text = paste0("list(", learner_hyper_fixed, ")")
                          )))
      }
    
    reso = eval(parse(text = paste0("c(", gsub('"', "", resolution),")")))
    names(reso) = ParamHelpers::getParamIds(ps)
    ctrl = makeTuneControlGrid(resolution = reso)
    
    TuneWrapper(lrn, inner_resampling, ps, ctrl)
    
    # learner + Filter + Tuning
  } else if (FeatSelFilter & !FeatSelWrapper & Tuning) {
    par_fw.perc = tune_fw.perc(fw.perc_upper)
    
    if (is.na(learner_hyper_tune)) {
      ps = eval(parse(
        text = paste0("makeParamSet(", featSel_hyper_tune_in, ")")
      ))
    }else{
      ps = eval(parse(
        text = paste0("makeParamSet(", learner_hyper_tune, ", ", featSel_hyper_tune_in, ")")
      ))
    }
    
    # combine featSel tuned out and in
    if(!is.na(featSel_hyper_tune_out) & !is.na(featSel_hyper_fixed)){
      featSel_hyper_fixed = paste0(featSel_hyper_fixed, ", ", featSel_hyper_tune_out)
    }
    
    if (is.na(learner_hyper_fixed)) {
      if (is.na(featSel_hyper_fixed)){
        lrn = makeFilterWrapper(
        learner = makeLearner(learner.name, id = learner.id),
        fw.method = featSel.name
      )
      }else{
        lrn = makeFilterWrapper(
          learner = makeLearner(learner.name, id = learner.id),
          fw.method = featSel.name,
          fw.fun.args = eval(parse(text = paste0("list(", featSel_hyper_fixed, ")")))
        )
      }
      
      } else{
          if (is.na(featSel_hyper_fixed)){
            lrn = makeFilterWrapper(
              learner = makeLearner(learner.name, id = learner.id,
                                    par.vals = eval(parse(
                                      text = paste0("list(", learner_hyper_fixed, ")")
                                    ))),
              fw.method = featSel.name
            )
          } else{
            lrn = makeFilterWrapper(
              learner = makeLearner(learner.name, id = learner.id,
                                    par.vals = eval(parse(
                                      text = paste0("list(", learner_hyper_fixed, ")")
                                    ))),
              fw.method = featSel.name,
              fw.fun.args = eval(parse(text = paste0("list(", featSel_hyper_fixed, ")")))
            )
          
        }
        
        
        
      }
    reso = eval(parse(text = paste0("c(", gsub('"', "", resolution),")")))
    names(reso) = ParamHelpers::getParamIds(ps)
    ctrl = makeTuneControlGrid(resolution = reso)
    
    TuneWrapper(lrn, inner_resampling, ps, ctrl)
    
    # Learner + Wrapper
  } else if (!FeatSelFilter & FeatSelWrapper & !Tuning) {
    if (is.na(learner_hyper_fixed)) {
      lrn = makeLearner(learner.name, id = learner.id)
    }else{
      lrn = makeLearner(learner.name, id = learner.id,
                        par.vals = eval(parse(
                          text = paste0("list(", learner_hyper_fixed, ")")
                        )))
    }
    
    makeFeatSelWrapper(
      learner = lrn,
      control = makeFeatSelControlSequential(
        method = featSel.name,
        alpha = 0.01,
        beta = -0.001,
        maxit = 50
      ),
      resampling = inner_resampling,
      show.info = FALSE
    )
    
    # Learner + Wrapper + tuning
  }else if (!FeatSelFilter & FeatSelWrapper & Tuning) {
    if (is.na(learner_hyper_fixed)) {
      lrn = makeLearner(learner.name, id = learner.id)
    }else{
      lrn = makeLearner(learner.name, id = learner.id,
                        par.vals = eval(parse(
                          text = paste0("list(", learner_hyper_fixed, ")")
                        )))
    }
    
      ps = eval(parse(
        text = paste0("makeParamSet(", learner_hyper_tune, ")")
      ))
      
      reso = eval(parse(text = paste0("c(", gsub('"', "", resolution),")")))
      names(reso) = ParamHelpers::getParamIds(ps)
      ctrl = makeTuneControlGrid(resolution = reso)
      
      lrn = TuneWrapper(lrn, inner_resampling, ps, ctrl)
      
    makeFeatSelWrapper(
      learner = lrn,
      control = makeFeatSelControlSequential(
        method = featSel.name,
        alpha = 0.01,
        beta = -0.001,
        maxit = 50
      ),
      resampling = inner_resampling,
      show.info = FALSE
    )
  }
}

combine_learners <- function(
  learner_list,
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
){
  learners = list()
  for (i in 1:nrow(learner_list)){
    learner = createLearner(
      learner.name = learner_list[i, ]$learner.name,
      learner.id = learner_list[i, ]$learner.id,
      featSel.name = learner_list[i, ]$featSel.name,
      learner_hyper_fixed = learner_list[i, ]$learner_hyper_fixed,
      learner_hyper_tune = learner_list[i, ]$learner_hyper_tune,
      featSel_hyper_fixed = learner_list[i, ]$featSel_hyper_fixed,
      featSel_hyper_tune_in = learner_list[i, ]$featSel_hyper_tune_in,
      fw.perc_upper = learner_list[i, ]$fw.perc_upper,
      featSel_hyper_tune_out = learner_list[i, ]$featSel_hyper_tune_out,
      FeatSelFilter = learner_list[i, ]$FeatSelFilter,
      FeatSelWrapper = learner_list[i, ]$FeatSelWrapper,
      Tuning = learner_list[i, ]$Tuning,
      resolution = learner_list[i, ]$Resolution,
      inner_resampling = inner_resampling,
      # par_fw.perc = par_fw.perc,
      par_nfolds = par_nfolds,
      par_glm_alpha = par_glm_alpha,
      par_SRC_nodesize = par_SRC_nodesize,
      par_min_node_size = par_min_node_size,
      par_mtry = par_mtry,
      par_nrounds = par_nrounds,
      par_max_depth = par_max_depth,
      par_xg_eta = par_xg_eta,
      par_lambda = par_lambda,
      par_XG_alpha = par_XG_alpha
    )
    
    learners = append(learners , list(learner))
  }
  learners
}

