# factor of accessibility based on distance to work and education
accessibility_factor_dist_all_func <- function(raw_dat){
  raw_dat %>% 
    rowwise() %>% 
    mutate(acc1 = if_else(event == 1, mean(c_across(starts_with("tt_"))), -10)) %>% 
    mutate(acc2 = if_else(event == 0, mean(c_across(starts_with("tt_"))), -10)) %>% 
    select(c(id.survey, acc1, acc2)) %>%  
    group_by(id.survey) %>% 
    mutate(acc1 = max(acc1)) %>% 
    mutate(acc2 = max(acc2)) %>% 
    ungroup() %>% 
    unique() %>% 
    mutate(delta_a = acc2 - acc1) %>% 
    select(id.survey, delta_a) %>% 
    arrange(id.survey)
  
}

accessibility_factor_dist_car_func <- function(raw_dat){
  raw_dat %>% 
    rowwise() %>% 
    mutate(acc1 = if_else(event == 1, mean(c_across(ends_with("car_mean"))), -10)) %>% 
    mutate(acc2 = if_else(event == 0, mean(c_across(ends_with("car_mean"))), -10)) %>% 
    select(c(id.survey, acc1, acc2)) %>%  
    group_by(id.survey) %>% 
    mutate(acc1 = max(acc1)) %>% 
    mutate(acc2 = max(acc2)) %>% 
    ungroup() %>% 
    unique() %>% 
    mutate(delta_a = acc2 - acc1) %>% 
    select(id.survey, delta_a) %>% 
    arrange(id.survey)
  
}

accessibility_factor_dist_pt_func <- function(raw_dat){
  raw_dat %>% 
    rowwise() %>% 
    mutate(acc1 = if_else(event == 1, mean(c_across(ends_with("pt_mean"))), -10)) %>% 
    mutate(acc2 = if_else(event == 0, mean(c_across(ends_with("pt_mean"))), -10)) %>% 
    select(c(id.survey, acc1, acc2)) %>%  
    group_by(id.survey) %>% 
    mutate(acc1 = max(acc1)) %>% 
    mutate(acc2 = max(acc2)) %>% 
    ungroup() %>% 
    unique() %>% 
    mutate(delta_a = acc2 - acc1) %>% 
    select(id.survey, delta_a) %>% 
    arrange(id.survey)
  
}

accessibility_factor_acc_func <- function(raw_dat, mode){
  acc_col = paste0("accessibility_", mode)
  
  raw_dat %>% 
    rowwise() %>% 
    mutate(acc1 = if_else(event == 1, !!sym(acc_col),-10L)) %>% 
    mutate(acc2 = if_else(event == 0, !!sym(acc_col),-10L)) %>% 
    select(c(id.survey, acc1, acc2)) %>%  
    group_by(id.survey) %>% 
    mutate(acc1 = max(acc1)/10e3) %>% 
    mutate(acc2 = max(acc2)/10e3) %>% 
    ungroup() %>% 
    unique() %>% 
    mutate(delta_a = (acc2 - acc1)) %>% 
    select(id.survey, delta_a, acc1, acc2) %>% 
    arrange(id.survey)
  
}

attitude_lm <- function(raw_dat, delta_a){
  dat = raw_dat %>% 
    filter(event == 1) %>% 
    left_join(delta_a, by = "id.survey")
  
 
  lm_dat = lm(acc2 ~
                acc1 +
                hhIncomeInStart +
                jobIsProfessionalSum +
                # shop_drive +
                under18 +
                commute_drive +
                tt_work_car_mean +
                
                rB3_f4 + rB3_f5 +
                rB4_f3 +
                rB4_f4 +
                rB4_f5 +
                rB5_f4 + rB5_f5 +
                rB5_f3 +
                # unimp_rB5 + 
                # rB11_f4 +
                rB11_f5 +
                rB12_f4 +
                # unimp_rB12 +
                rB12_f3 +
                rB12_f5 +
                # rB13_f4 +
                rB13_f5
                
                , data = dat)
  lm_dat
  
}

add_acc2_for_censored <- function(raw_dat, lm_dat){
  dat = raw_dat %>% 
    rename(acc1 = accessibility_car) %>% 
    mutate(acc1 = acc1 / 10e3)

  acc2_pred = predict(lm_dat, new = dat)
  
  cbind(dat, acc2_pred)
  
}