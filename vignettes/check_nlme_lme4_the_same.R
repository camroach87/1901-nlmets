library(microbenchmark)
library(nlme)
library(lme4)

#### Data -------------------------
fcst_date <- ymd("2017-01-06")  #fcst_test_dates[100]
fcst_period <- 17
train_window <- fcst_train_window
train_window <- fcst_train_window
min_train_days <- fcst_min_train_days
data_list <- train_test_split(main_df, fcst_date, fcst_period, train_window,
                              min_train_days)
list2env(data_list, env = environment())

wvars <- fs_df %>% 
  filter(month == month(fcst_date),
         period == fcst_period) %>% 
  pull(vars) %>% 
  pluck(1)

terms <- get_terms(wvars)


#### Random intercept model --------
nlme_ri <- lme(as.formula(paste("log(wh) ~ ", terms)),
                 data = train_df,
                 random = ~ 1 | bid,
                 control = lmeControl(opt = "optim"))
lme4_ri <- lmer(paste("log(wh) ~ (1|bid) +", terms),
                  data = train_df)

# Compare random effects
as.data.frame(ranef(lme4_ri)) %>% 
  select(bid = grp, lme4 = condval) %>% 
  full_join(as.data.frame(ranef(nlme_ri)) %>% 
              rownames_to_column("bid") %>% 
              rename(nlme = `(Intercept)`)) %>% 
  mutate(abs_diff = abs(lme4 - nlme)) %>% 
  arrange(desc(abs_diff))

# Compare fixed effects
as.data.frame(fixef(lme4_ri)) %>% 
  rownames_to_column() %>% 
  rename_all(vars(c('terms', 'lme4'))) %>% 
  full_join(as.data.frame(fixef(nlme_ri)) %>% 
              rownames_to_column() %>% 
              rename_all(vars(c('terms', 'nlme')))) %>% 
  mutate(abs_diff = abs(lme4 - nlme)) %>% 
  arrange(desc(abs_diff))


# Benchmark
# microbenchmark(lme(as.formula(paste("log(wh) ~ ", terms)), data = train_df, random = ~ 1 | bid, control = lmeControl(opt = "optim")),
#                # lme(as.formula(paste("log(wh) ~ ", terms)), data = train_df, random = ~ 1 | bid),
#                lmer(paste("log(wh) ~ (1|bid) +", terms), data = train_df))


#### Random slope model ----------------
nlme_ris <- lme(as.formula(paste("log(wh) ~ ", terms)),
                 data = train_df,
                 random = ~ scaled_temperature | bid,
                 control = lmeControl(opt = "optim"))
lme4_ris <- lmer(paste("log(wh) ~ (scaled_temperature|bid) +", terms),
                  data = train_df)



# Compare random effects
as.data.frame(ranef(lme4_ris)) %>% 
  select(grp, term, lme4 = condval) %>% 
  full_join(as.data.frame(ranef(nlme_ris)) %>% 
              rownames_to_column("grp") %>% 
              gather(term, nlme, -grp)) %>% 
  mutate(abs_diff = abs(lme4 - nlme)) %>% 
  arrange(desc(abs_diff)) %>% 
  head()

# Compare fixed effects
as.data.frame(fixef(lme4_ris)) %>% 
  rownames_to_column() %>% 
  rename_all(vars(c('terms', 'lme4'))) %>% 
  full_join(as.data.frame(fixef(nlme_ris)) %>% 
              rownames_to_column() %>% 
              rename_all(vars(c('terms', 'nlme')))) %>% 
  mutate(abs_diff = abs(lme4 - nlme)) %>% 
  arrange(desc(abs_diff)) %>% 
  head()


# Benchmark
# microbenchmark(lme(as.formula(paste("log(wh) ~ ", terms)), data = train_df, random = ~ scaled_temperature | bid, control = lmeControl(opt = "optim")),
#                lmer(paste("log(wh) ~ (scaled_temperature|bid) +", terms), data = train_df))



### SSC no correlation structure -------
nlme_ssc <- lme(as.formula(paste("log(wh) ~ ", terms)),
                 data = train_df,
                 random = ~ ns(scaled_temperature, df = 3) | bid,
                 control = lmeControl(opt = "optim")) 
lme4_ssc <- lmer(paste("log(wh) ~ (ns(scaled_temperature, df = 3)|bid) +", terms),
     data = train_df)


# Compare random effects
as.data.frame(ranef(lme4_ssc)) %>% 
  select(grp, term, lme4 = condval) %>% 
  full_join(as.data.frame(ranef(nlme_ssc)) %>% 
              rownames_to_column("grp") %>% 
              gather(term, nlme, -grp)) %>% 
  mutate(abs_diff = abs(lme4 - nlme)) %>% 
  arrange(desc(abs_diff)) %>% 
  head()

# Compare fixed effects
as.data.frame(fixef(lme4_ssc)) %>% 
  rownames_to_column() %>% 
  rename_all(vars(c('terms', 'lme4'))) %>% 
  full_join(as.data.frame(fixef(nlme_ssc)) %>% 
              rownames_to_column() %>% 
              rename_all(vars(c('terms', 'nlme')))) %>% 
  mutate(abs_diff = abs(lme4 - nlme)) %>% 
  arrange(desc(abs_diff)) %>% 
  head()



### SSC AR1 correlation structure -------
# test_nlme_ar1 <- lme(as.formula(paste("log(wh) ~ ", terms)),
#                  data = train_df,
#                  random = ~ ns(scaled_temperature, df = 3) | bid,
#                  correlation = corAR1(),
#                  control = lmeControl(opt = "optim"))
nlme_ssc_ar1 <- update(nlme_ssc, correlation = corAR1())

BIC(nlme_ri,
    nlme_ris,
    nlme_ssc,
    nlme_ssc_ar1)


# microbenchmark(
#   lme(as.formula(paste("log(wh) ~ ", terms)), data = train_df,
#       random = ~ ns(scaled_temperature, df = 3) | bid, correlation = corAR1(), 
#       control = lmeControl(opt = "optim")),
#   update(nlme_ssc, correlation = corAR1()),
#   times = 10
# )




#### AR1 error for period 17, 2017-01-06
terms <- get_terms("scaled_temperature_min")
form <- as.formula(paste("log(wh) ~ ", terms))
lme(form,
    data = train_df,
    random = ~ ns(scaled_temperature_lag_48, df = 3) | bid,
    correlation = corAR1(),
    method = "ML",
    control = lmeControl(opt = "optim",
                         msMaxIter=100,
                         returnObject = TRUE))
