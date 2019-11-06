#' Loads all building data
#' 
#' Creates a data frame containing building demand and attributes.
#'
#' @param attribute_file 
#' @param qh_file 
#' @param building_list_file
#' @param business_days
#'
#' @return
#' @export
#'
#' @examples
load_all_data <- function(attribute_file, qh_path, building_list_file,
                          business_days) {
  cat("Loading attributes...\n\n")
  # Already tidied file. Use ba_helpers::read_ba_attributes if you need to tidy original file.
  attributes <- read_csv(attribute_file)
  
  cat("Loading quarter hourly data...\n\n")
  qh_data <- combine_qh_csv(qh_path)
  
  cat("Loading building list...\n\n")
  buildings <- read_csv(building_list_file, comment = "#") %>% 
    select(b_uid = UID,
           BID = IDNUMBER,
           Name = NAME,
           NetLettableArea = NETLETTABLEAREA,
           ASSETTYPE_UID,
           CITY,
           STATE)
  
  cat("Cleaning data and generating features...\n\n")
  all_data <- attributes %>% 
    inner_join(buildings) %>% 
    inner_join(qh_data) %>% 
    rename_all(tolower) %>% 
    mutate(date = date(ts),
           period = (hour(ts)+minute(ts)/60)*4 + 1,
           wh = kwh/netlettablearea*1e3) %>% 
    filter(assettype_uid == 9,  # Office buildings. Not including 5 - different type.
           state %in% c("ACT", "NSW", "QLD", "SA", "VIC", "WA"),  # Australia only
           wh > 0, # Remove obvious outliers
           wh < 25) %>%  
    as_tsibble(key = id(bid), index = ts) %>%
    fill_gaps() %>%
    group_by(bid) %>%
    # Exclude wh to avoid prolonged stretches where no data is available for a
    # building (gap in service). Can cause issues with model.matrix. These
    # missing stretches will be filtered out by na.omit().
    fill(-wh, .direction = "down") %>%
    mutate(temperature_lag_12 = lag(temperature, 12),
           temperature_lag_24 = lag(temperature, 24),
           temperature_lag_48 = lag(temperature, 48),
           temperature_lag_72 = lag(temperature, 72),
           temperature_lag_96 = lag(temperature, 96),
           temperature_lag_192 = lag(temperature, 192),
           temperature_lag_288 = lag(temperature, 288),
           temperature_max = slide_dbl(temperature, max, .size = 96),
           temperature_min = slide_dbl(temperature, min, .size = 96),
           temperature_avg = slide_dbl(temperature, mean, .size = 288))
  
  if (business_days) {
    cat("Filtering for business days...\n\n")
    all_data <- filter_business_days(all_data, outlier_file)
    
    # Add demand lags. Weekend demand values not used.
    all_data <- all_data %>%
      group_by(bid) %>% 
      mutate(wh_lag_96 = lag(wh, 96),
             wh_lag_192 = lag(wh, 96*2),
             wh_lag_672 = lag(wh, 96*5))
  } else {
    # Add demand lags. Weekend demand values not used.
    all_data <- all_data %>%
      group_by(bid) %>% 
      mutate(wh_lag_96 = lag(wh, 96),
             wh_lag_192 = lag(wh, 96*2),
             wh_lag_672 = lag(wh, 96*7))

  }
  
  # Remove NAs and anonymise
  all_data <- all_data %>% 
    ungroup() %>% 
    na.omit() %>% 
    select(-name)
  
  all_data
}


#' Load and combine building csv files
#'
#' Saves the output tibble to disk as an .RData file. Duplicated rows are
#' omitted - these come straight from the database so there is an issue with
#' data storage. Not much can be done about that unfortunately.
#'
#' @param qh_path
#'
#' @return
#' @export
#'
#' @examples
combine_qh_csv <- function(qh_path) {
  tibble(file = list.files(qh_path, full.names = TRUE)) %>% 
    mutate(data = map(file, read_csv,
                      col_types = cols_only(
                        BUILDINGUID = col_integer(),
                        DT = col_datetime(format = ""),
                        WEEKDAY = col_character(),
                        KWH = col_double(),
                        TEMPERATURE = col_double(),
                        MOISTURE = col_double()
                      ))) %>% 
    select(data) %>% 
    unnest() %>% 
    rename_all(tolower) %>% 
    select(ts = dt, b_uid = buildinguid, kwh, temperature, moisture) %>% 
    na.omit() %>%
    filter(!are_duplicated(., key=id(b_uid), index = ts))
}


#' Filter for business days
#'
#' @param data 
#' @param outlier_file 
#'
#' @return
#' @export
#'
#' @examples
filter_business_days <- function(data, outlier_file) {
  outlier_dates <- read_csv(outlier_file) %>% 
    mutate(DATE = date(DATE)) %>% 
    rename_all(tolower) %>% 
    select(b_uid = buildinguid, date)
  
  data %>% 
    filter(wday(date) %in% 2:6) %>%  # remove weekends
    anti_join(outlier_dates, by = c("b_uid", "date"))  # remove public holidays
}


#' Get train and test split
#'
#' Creates a train and test dataframe by filtering for the given date and
#' 15-minute period of day. There is also a check to ensure a minimum number of
#' training days are present for each building.
#'
#' @param data
#' @param filter_date
#' @param filter_period
#' @param train_window
#' @param min_train_days
#' @param train_only logical. Indicates if only training data frame should be
#'   returned.
#'
#' @return
#' @export
#'
#' @examples
train_test_split <- function(data, filter_date, filter_period, train_window, 
                             min_train_days, train_only = FALSE) {
  train_df <- main_df %>% 
    filter(date < filter_date,
           date > filter_date - train_window,
           period == filter_period)
  
  bid_filter <- train_df %>% 
    count(bid) %>% 
    filter(n < min_train_days) %>% 
    pull(bid)
  
  train_df <- filter(train_df, !(bid %in% bid_filter))
  
  if (train_only) {
    output = list(train_df = train_df)
  } else {
    test_df <- main_df %>% 
      filter(date == filter_date,
             period == filter_period,
             bid %in% unique(train_df$bid))  # need same levels
    
    output = list(train_df = train_df,
                  test_df = test_df)
  }
  
  output
}
