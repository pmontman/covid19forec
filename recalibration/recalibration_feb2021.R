library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

#load the data from the internet.
#be careful, for reproducibility, the original file of the day should be kept
#because of potential retroactive changes. and filter the date
RETRIEVE_DATA = FALSE
if (RETRIEVE_DATA) {
  other_csv <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  other_csv = other_csv %>% select(-(3:4)) %>%
    mutate(region=paste(`Province/State`, `Country/Region`)) %>% select(-(1:2))

  other_csv = other_csv %>% pivot_longer(cols=-ncol(other_csv), values_to="cases")
  other_csv = rename(other_csv, date=name)
  other_csv <- mutate(other_csv, date = as_date(as_datetime(date, format = "%m/%d/%y")) )
  other_csv
  write_csv(other_csv, "16Feb_cases_JH.csv")
}



JH_cases = read_csv("16Feb_cases_JH.csv")




#transform to a list of time series MCOMP style
cases_list = JH_cases %>% select(region, date, cases) %>% group_by(region) %>%
  group_map( function(tbl, y) {
    tbl = tbl %>% arrange(date)
    list(x=tbl$cases,
         id=y$region,
         lastdate=tail(tbl$date,1))
  }
  )

length(cases_list)


#differenciate to use new daily cases
cases_list = lapply(cases_list, function(ll) {
  ll$x = diff(ll$x,differences = 1)
  ll$x[ll$x < 0] = 0
  #if (length(grep("Australia",ll$id))<1) {
  #  ll$x = stats::filter(ll$x, rep(1/3,3), sides=1)
  #  ll$x = ll$x[!is.na(ll$x)]
  #}
  ll
})

#filter the one that had in one day over 50 new cases
#to remove problematic time series
cases_list= Filter( function (ll) max(ll$x > 50) || grep("Australia", ll$id), cases_list)

#filter the one that have a maximum daily jump in new cases of less than a third of the maximum
#arbitrary idea to filter problematic time series
cases_list= Filter( function (ll) {(max(diff(ll$x)) < max(ll$x)*0.33) || grep("Australia", ll$id)},
            cases_list)


#prepare the time series for modeling (with evaluation holdout of horiz days)
#calculate forecasts of base models for comparison purposes
cases_list = prepare_forecasts(cases_list, horiz = 14*1)

#set negative forecasts to 0
cases_list = lapply(cases_list, function(ll) {
  ll$ff[ll$ff < 0] = 0
  ll
})

#hyperparameters for the linear model, types of normalization of the series
#for the Australian case, do not change them!
#because we are required to do a log-linear model
#and it is also easier to explain
do_MASE = FALSE
do_log = TRUE
do_maxnorm = FALSE
do_inverse_trans = FALSE


res = analyze_glob_model(cases_list, linear_model,  lag_range=1:100,
                         MASE_norm = do_MASE,
                         ret_series = TRUE, do_log_transform = do_log,
                         do_max_scale = do_maxnorm,
                         do_inversion = do_inverse_trans)
res
plot(as.numeric(res[1,-(1:3)]),type="l")

which.min(res[1,-(1:3)])
which.min(res[2,-(1:3)])
which.min(res[3,-(1:3)])

orig_coef = attr(res, "last_model")$coef


#FILTER the Australia case only

focus_series = attr(res, "forec_series")
focus_series = Filter(function (ll) grepl("Australia", ll$id),
                      focus_series)

err_aus = calc_error_summary(focus_series)
err_aus

which.min(err_aus[1,-(1:3)])
which.min(err_aus[2,-(1:3)])
which.min(err_aus[3,-(1:3)])

plot(as.numeric(err_aus[3,-(1:3)]),type="l")


############################################
###### output the ##########################
###### coefficients ########################
##### retraining to the more recent data ###
############################################

best_method = 27 #EYEBALLING I WOULD CHOOSE SOMETHING LIKE an AR 27

horiz = 28
cases_toff = lapply(cases_list, function(ll) {
  ll$x = ts(c(ll$x, ll$xx))
  ll$xx = ts(rep(0, horiz))
  ll$ff = NULL
  ll
})



res = analyze_glob_model(cases_toff, linear_model, MASE_norm = do_MASE,
                         ret_series = TRUE, do_log_transform = do_log,
                         lag_range = 1:best_method,
                         do_max_scale = do_maxnorm)


#the coefficients of the model
coefficients = attr(res, "last_model")$coef

#output the coefficeints of the model
dput(as.vector(coefficients))

