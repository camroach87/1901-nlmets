# Note: predict.lme throws errors if formula is created in `lme` function call.
# Need to use eval and substitute to get around this.
# Reference: https://stat.ethz.ch/pipermail/r-help/2003-January/029199.html


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
  form <- as.formula(paste("log(wh) ~ ", terms))
  
  eval(substitute(
    lme(form,
        data = data,
        random = ~ 1 | bid,
        method = "ML",
        control = lmeControl(opt = "optim",
                             msMaxIter=100,
                             returnObject = TRUE)),
    list(form=form)
  ))
}


fit_ris <- function(data, wvars) {
  terms <- get_terms(wvars)
  form <- as.formula(paste("log(wh) ~ ", terms))
  
  eval(substitute(
    lme(form,
        data = data,
        random = ~ scaled_temperature | bid,
        method = "ML",
        control = lmeControl(opt = "optim",
                             msMaxIter=100,
                             returnObject = TRUE)),
    list(form=form)
  ))
}


fit_ssc <- function(data, wvars) {
  terms <- get_terms(wvars)
  form <- as.formula(paste("log(wh) ~ ", terms))
  
  eval(substitute(
    lme(form,
        data = data,
        random = ~ ns(scaled_temperature, df = 3) | bid,
        method = "ML",
        control = lmeControl(opt = "optim",
                             msMaxIter=100,
                             returnObject = TRUE)),
    list(form=form)
  ))
}


fit_ssc_attr <- function(data, wvars) {
  terms <- get_terms(wvars)
  form <- as.formula(paste("log(wh) ~ ", terms, 
                           "+ basebldngfeedonly + dxsystem + electricelementheating",
                           "+ centraldist"))
  
  eval(substitute(
    lme(form,
        data = data,
        random = ~ ns(scaled_temperature, df = 3) | bid,
        method = "ML",
        control = lmeControl(opt = "optim",
                             msMaxIter=100,
                             returnObject = TRUE)),
    list(form=form)
  ))
}


get_terms <- function(wvars) {
  # Remove scaled_temperature as it is modelled as a subject specific curve
  wvars <- wvars[wvars!="scaled_temperature"]
  
  if (length(wvars) > 0) {
    terms <- paste(paste0("ns(", wvars, ", df = 3)"), collapse = " + ")
  } else {
    terms <- 1  # intercept only
  }
  
  terms
}


# fit_ssc_ar1 <- function(data, wvars) {
#   terms <- get_terms(wvars)
#   form <- as.formula(paste("log(wh) ~ ", terms))
#   
#   eval(substitute(
#     lme(form,
#         data = data,
#         random = ~ ns(scaled_temperature, df = 3) | bid,
#         correlation = corAR1(),
#         method = "ML",
#         control = lmeControl(opt = "optim",
#                              msMaxIter=100,
#                              returnObject = TRUE)),
#     list(form=form)
#   ))
# }
# 
# 
# fit_ssc_ar1_attr <- function(data, wvars) {
#   terms <- get_terms(wvars)
#   form <- as.formula(paste("log(wh) ~ ", terms, 
#                            "+ basebldngfeedonly + dxsystem + electricelementheating",
#                            "+ centraldist"))
#   
#   eval(substitute(
#     lme(form,
#         data = data,
#         random = ~ ns(scaled_temperature, df = 3) | bid,
#         correlation = corAR1(),
#         method = "ML",
#         control = lmeControl(opt = "optim",
#                              msMaxIter=100,
#                              returnObject = TRUE)),
#     list(form=form)
#   ))
# }



#' Fit SSCAR(1) model
#' 
#' Fits a subject specific curve model with autocorrelation structure for residuals.
#'
#' TODO: Needs a predict method for the sscar1 class.
#'   - Should be returned from this function as its own class.
#'   - In the predict function create the new Z and Z.subject matrices. Based off training knot positions.
#'   - Add ident as a variable in the dataframe
#'   - Make sure knot positions in the test data are the same as for the test. Should 
#'     probably include these knots as an attribute
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
# fit_sscar1 <- function(data) {
#   data$ident <- 1
#   x <- as.numeric(data$scaled_temperature)
#   K <- 2
#   K.subject <- 1
#   
#   knots <- quantile(unique(x), seq(0,1,length=K+2))[-c(1,K+2)]
#   # Z <- outer(x, knots, "-")
#   Z <- outer(x, knots, "-")
#   Z <- (Z*(Z>0))^3
#   knots.subject <- quantile(unique(x), seq(0, 1, length=K.subject+2))
#   knots.subject <- knots.subject[-c(1,K.subject+2)]
#   Z_sub <- outer(x, knots.subject, "-")
#   Z_sub <- (Z_sub*(Z_sub>0))^3
#   
#   # TODO: Convert Z and Z_sub to dataframes. Rename columns automatically and then column bind to data.
#   data$Z1 <- Z[,1]
#   data$Z2 <- Z[,2]
#   data$Z_sub <- Z_sub
#   
#   fit <- nlme::lme(wh ~ poly(scaled_temperature, 2),
#     data = data,
#     random = list(ident = pdIdent(~Z1+Z2-1),
#                   bid = pdSymm(~scaled_temperature),
#                   bid = pdIdent(~Z_sub-1)),
#     correlation = corAR1(value = .5),
#     control = lmeControl(opt = "optim"))
#   
#   fit
# }