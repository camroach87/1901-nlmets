#' Fit and test models
#'
#' @param fcst_date date. Date to carry out one day ahead forecast for.
#' @param fcst_period integer. 15-minute period of day to carry out forecast
#'   for.
#' @param scenario_dates vector of dates to carry out scenario analysis for. 
#'   Scenarios for all buildings are returned. If NULL, no scenario analysis 
#'   will run. Defaults to NULL.
#' @param train_window integer. Length of training window.
#' @param min_train_days integer. Minimum number of days that must be included
#'   for each building. Buildings without enough days are removed from the
#'   training and test data frames to avoid fitting issues.
#' @param return_fits logical. Defaults to FALSE as lots of models can result
#'   in memory issues.
#'
#' @return
#' @export
#'
#' @examples
test_models <- function(fcst_date, fcst_period, train_window, min_train_days,
                        scenario_dates = NULL, return_fits = FALSE) {
  cat(paste0("Fitting models for period ", fcst_period, ", ", fcst_date, ".\n"))
  
  data_list <- train_test_split(main_df, fcst_date, fcst_period, train_window,
                                min_train_days)
  list2env(data_list, env = environment())
  
  predictors <- fs_df %>% 
    filter(month == month(fcst_date),
           period == fcst_period) %>% 
    pull(vars) %>% 
    pluck(1)
  
  # Add models we wish to fit here
  model_df <- list(Naive = fit_naive(train_df),
                   ILR = fit_ind_lm(train_df),
                   INS = fit_ind_ns(train_df, "scaled_temperature"),
                   PR = fit_pool(train_df, predictors),
                   RI = fit_ri(train_df, predictors),
                   RIS = fit_ris(train_df, predictors),
                   SSC = fit_ssc(train_df, predictors),
                   SSCATTR = fit_ssc_attr(train_df, predictors)) %>% 
    map(list) %>% 
    as_tibble() %>% 
    gather(model, fit)
  
  predict_df <- model_df %>%
    mutate(pred = map(fit, predict, newdata = test_df),
           pred = map2(fit, pred, back_transform_log)) %>%
    select(-fit) %>%  # reduce memory usage
    unnest(pred) %>% 
    bind_cols(map_dfr(1:nrow(model_df),
                      ~ test_df %>% 
                        select(-date, -period))) %>% 
    mutate(resid = pred-wh) %>% 
    select(model, pred, resid, everything())
  
  
  if (fcst_date %in% scenario_dates) {
    # Set up scenarios
    scenario_list <- test_df %>% 
      list("original" = .,
           "basebldngfeed" = mutate(., basebldngfeedonly = !basebldngfeedonly),
           "dxsystem" = mutate(., dxsystem = !dxsystem),
           "electricelementheating" = mutate(., electricelementheating = !electricelementheating),
           "centraldist" = mutate(., centraldist = !centraldist))
    
    scenario_df <- scenario_list %>% 
      map(function(x) {
        x %>% 
          mutate(pred = predict(model_df %>% 
                                  filter(model == "SSCATTR") %>% 
                                  pull(fit) %>% 
                                  pluck(1),
                                newdata = .),
                 pred = exp(pred)) %>% 
          select(ts, bid, wh, pred, !!building_attributes)
      }) %>% 
      bind_rows(.id = "scenario")
  }  else {
    scenario_df <- NULL
  }
  
  output <- tibble(
    preds = list(predict_df),
    scenarios = list(scenario_df)
  )
  
  if (return_fits) {
    output$fits <- list(model_df)
  }
  
  output
}


#' Back transform predictions with log transform
#'
#' Checks if log transform was used in model formulation for response and back 
#' transforms if it was. Works for lm, ind_lm and lmerMod objects.
#'
#' @param object model object
#' @param pred vector of predictions to be back transformed.
#'
#' @return
#' @export
#'
#' @examples
back_transform_log <- function(object, pred) {
  UseMethod("back_transform_log")
}

back_transform_log.ind_naive <- function(object, pred) {
  pred
}

back_transform_log.ind_lm <- function(object, pred) {
  log_transform <- str_detect(as.character(object$fit[[1]]$call$formula)[[2]],
                              "^log\\(.+\\)")
  
  if (log_transform) pred = exp(pred)
  
  pred
}

back_transform_log.lm <- function(object, pred) {
  log_transform <- any(str_detect(as.character(object$call$formula), 
                                  "^log\\({1}.+\\){1}.\\~"))
  
  if (log_transform) pred = exp(pred)
  
  pred
}

back_transform_log.lmerMod <- function(object, pred) {
  log_transform <- str_detect(as.character(object@call$formula)[[2]], 
                              "^log\\(.+\\)")
  
  if (log_transform) pred = exp(pred)
  
  pred
}
