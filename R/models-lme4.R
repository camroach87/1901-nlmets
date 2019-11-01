# Predicts most recent observation for each building
fit_naive <- function(data) {
  last_obs <- data %>% 
    group_by(bid) %>% 
    arrange(desc(ts)) %>% 
    slice(1) %>% 
    select(bid, wh)
    
  output <- list(
    method = "Individual naive models",
    mean = last_obs
  )
  
  class(output) <- "ind_naive"
  
  output
}

predict.ind_naive <- function(object, newdata) {
  # TODO: Throw error if there is a bid in newdata that isn't in object$mean 
  #       data frame. Shouldn't happen if `train_test_split` used to create
  #       data for predictions.
  newdata %>% 
    select(bid) %>% 
    left_join(object$mean) %>% 
    pull(wh)
}

fit_ind_lm <- function(data) {
  output <- data %>% 
    group_by(bid) %>% 
    nest() %>% 
    mutate(fit = map(data, ~lm(log(wh) ~ poly(scaled_temperature, degree = 2),
                               data = .x))) %>% 
    select(bid, fit)
  
  class(output) <- c("ind_lm", "tbl_df", "tbl", "data.frame")
  
  output
}


fit_ind_ns <- function(data, wvars) {
  terms <- get_terms(wvars)
  
  output <- data %>% 
    group_by(bid) %>% 
    nest() %>% 
    mutate(fit = map(data, ~lm(paste("log(wh) ~", terms),
                               data = .x))) %>% 
    select(bid, fit)
  
  class(output) <- c("ind_lm", "tbl_df", "tbl", "data.frame")
  
  output
}


predict.ind_lm <- function(object, newdata) {
  # FIXME: if more than one value coming in here the values might get out of 
  #        alignment after nesting and unnesting.
  newdata %>% 
    group_by(bid) %>% 
    nest() %>% 
    inner_join(object, by = "bid") %>% 
    mutate(pred = map2(fit, data, ~ predict(.x, .y))) %>% 
    unnest(pred) %>% 
    pull(pred)
}


fit_pool <- function(data, wvars) {
  terms <- get_terms(wvars)
  
  lm(paste("log(wh) ~ bid +", terms),
     data = data)
}


fit_ri <- function(data, wvars) {
  terms <- get_terms(wvars)
  
  lmer(paste("log(wh) ~ (1|bid) +", terms),
       data = data,
       REML = FALSE)
}


fit_ris <- function(data, wvars) {
  terms <- get_terms(wvars)
  
  lmer(paste("log(wh) ~ (scaled_temperature|bid) +", terms),
       data = data,
       REML = FALSE)
}


fit_ssc <- function(data, wvars) {
  terms <- get_terms(wvars)
  
  lmer(paste("log(wh) ~ (ns(scaled_temperature, df = 3)|bid) +", terms),
       data = data,
       REML = FALSE)
}


fit_ssc_attr <- function(data, wvars) {
  terms <- get_terms(wvars)
  
  lmer(paste("log(wh) ~ (ns(scaled_temperature, df = 3)|bid) +", terms,
         "+ basebldngfeedonly + dxsystem + electricelementheating +",
         "centraldist"),
       data = data,
       REML = FALSE)
}


get_terms <- function(wvars) {
  if (length(wvars) > 0) {
    terms <- paste(paste0("ns(", wvars, ", df = 3)"), collapse = " + ")
  } else {
    terms <- 1  # intercept only
  }
  
  terms
}