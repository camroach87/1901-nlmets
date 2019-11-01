#' Carry out feature selection using pooled model
#'
#' @param fs_date date. Date to carry out feature selection for.
#' @param fs_period integer. 15-minute period of day to carry out feature
#'   selection for.
#' @param train_window integer. Length of training window.
#' @param min_train_days integer. Minimum number of days that must be included
#'   for each building. Buildings without enough days are removed from the
#'   training and test data frames to avoid fitting issues.
#'
#' @return
#' @export
#'
#' @examples
select_features <- function(fs_date, fs_period, train_window, min_train_days, 
                            ...) {
  cat(paste0("Selecting features for ", month(fs_date, T, F), 
             ", period ", fs_period, ".\n"))
  data_list <- train_test_split(main_df, fs_date, fs_period, train_window,
                                min_train_days, train_only = TRUE)
  list2env(data_list, env = environment())
  
  # FIXME: this should be passed as an input argument
  formula <- ~  
    bid +
    ns(scaled_temperature, df = 3) +
    ns(scaled_temperature_lag_12, df = 3) +
    ns(scaled_temperature_lag_24, df = 3) +
    ns(scaled_temperature_lag_48, df = 3) +
    ns(scaled_temperature_lag_72, df = 3) +
    ns(scaled_temperature_lag_96, df = 3) +
    ns(scaled_temperature_lag_192, df = 3) +
    ns(scaled_temperature_lag_288, df = 3) +
    ns(scaled_temperature_max, df = 3) +
    ns(scaled_temperature_min, df = 3) +
    ns(scaled_temperature_avg, df = 3) +
    ns(scaled_wh_lag_96, df = 3) +
    ns(scaled_wh_lag_192, df = 3) +
    ns(scaled_wh_lag_672, df = 3)
  
  best_subset(train_df, formula, "scaled_temperature", ...)
}


#' Select weather variables using best subset regression
#'
#' Selects weather variables given a training data farme and formula.
#'
#' @param train_df
#' @param formula
#' @param forced_vars character. Vector or string containing names of variables
#'   that are forced to be included in the formulation.
#' @param size The fraction of rows to select in training data frame. Should
#'   only be used when testing code - will result in confusing R^2 values where
#'   they sometimes aren't monotonically increasing due to the random selection
#'   from the full dataframe.
#' @param n_var_max Maximum number of variables to be included NOT including
#'   forced variables. Defaults to NULL which results in all subset sizes being
#'   considered.
#'
#' @return
#' @export
#'
#' @examples
best_subset <- function(train_df, formula, forced_vars = NULL, size = 1, 
                        n_var_max = NULL) {
  design_matrix <- as_tibble(model.matrix(formula, train_df))
  
  ftr_df <- tibble(name = attr(design_matrix, "names")) %>% 
    mutate(building_dummy = str_detect(name, "^bid"),
           weather_var = str_extract(name, "(?<=ns\\()[[:alnum:]_]*"),
           forced_var = if_else(weather_var %in% forced_vars, TRUE, FALSE))
  
  candidate_vars <- ftr_df %>% 
    filter(!is.na(weather_var),
           forced_var == FALSE) %>% 
    pull(weather_var) %>% 
    unique()
  n_var <- min(length(candidate_vars), n_var_max)
  
  cat("Forced variables are:", forced_vars, "\n")
  cat("Selecting from maximum of", n_var, "candidate variables. Testing: ")
  
  start_time <- Sys.time()
  best_list <- list()
  for (i in 0:n_var) {
    cat(paste0(i, " "))
    var_combn <- combn(candidate_vars, i, simplify = FALSE)
    var_combn <- map(var_combn, ~ c(forced_vars, .))  # add forced vars
    
    for (j in 1:length(var_combn)) {
      # Find best model with i candidate variables
      fit.dm_columns <- ftr_df %>% 
        filter(weather_var %in% var_combn[[j]]) %>% 
        pull(name) %>% 
        c(ftr_df %>%
            filter(building_dummy == TRUE) %>%
            pull(name)) %>%
        c("(Intercept)")
      
      fit.train_df <- tibble(wh = train_df$wh) %>% 
        bind_cols(design_matrix[, fit.dm_columns]) %>% 
        sample_frac(size)
      
      fit.lm <- lm(log(wh) ~ . + 0, data = fit.train_df)  # + 0 because intercept in design matrix
      
      best_list[[length(best_list) + 1]] <- 
        tibble(n_vars = i,
               combn = j,
               vars = list(var_combn[[j]]),
               r_squared = summary(fit.lm)$r.squared,
               aic = AIC(fit.lm),
               bic = BIC(fit.lm),
               loocv = mean((residuals(fit.lm)/(1 - hatvalues(fit.lm)))^2))
    }
  }
  end_time <- Sys.time()
  time_diff <- end_time - start_time
  cat(": Completed in", round(time_diff, 2), attr(time_diff, "units"), "\n")
  
  bind_rows(best_list) %>% 
    group_by(n_vars) %>% 
    filter(r_squared == max(r_squared)) %>% 
    ungroup()
}
