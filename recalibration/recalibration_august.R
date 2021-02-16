library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

other_csv <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
other_csv = other_csv %>% select(-(3:4)) %>%
  mutate(region=paste(`Province/State`, `Country/Region`)) %>% select(-(1:2))

other_csv = other_csv %>% pivot_longer(cols=-ncol(other_csv), values_to="cases")
other_csv = rename(other_csv, date=name)
other_csv <- mutate(other_csv, date = as_date(as_datetime(date, format = "%m/%d/%y")) )
other_csv

#write_csv(other_csv, "31Aug_cases_JH.csv")




JH_cases = read_csv("31Aug_cases_JH.csv")





#cases_list = curated_cases_list(JH_cases)
cases_list = JH_cases %>% select(region, date, cases) %>% group_by(region) %>%
  group_map( function(tbl, y) {
    tbl = tbl %>% arrange(date)
    list(x=tbl$cases,
         id=y$region,
         lastdate=tail(tbl$date,1))
  }
  )

length(cases_list)

#
#   cases_list = lapply(cases_list, function(ll) {
#     ll$x = tail(ll$x, 100)
#     ll
#   })


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

cases_list= Filter( function (ll) max(ll$x > 50) || grep("Australia", ll$id), cases_list)


cases_list= Filter( function (ll) {(max(diff(ll$x)) < max(ll$x)*0.333) || grep("Australia", ll$id)},
            cases_list)

# cases_list = lapply(cases_list, function(ll) {
#   ll$x = tail(ll$x, 120)
#   ll
# })





# #cases_list = Filter(function(ll) sum(ll$x < 0) < 1, cases_list)
#
# #cases_list = Filter(function(ll) sd(ll$x) > 0.01 || grep("Australia", ll$id), cases_list)


#calculate forecasts of base models for comparison purposes
cases_list = prepare_forecasts(cases_list, horiz = 14)

#set negative forecasts to 0
cases_list = lapply(cases_list, function(ll) {
  ll$ff[ll$ff < 0] = 0
  ll
})

do_MASE = FALSE
do_log = TRUE
do_maxnorm = FALSE
do_inverse_trans = FALSE


res = analyze_glob_model(cases_list, linear_model,  lag_range=1:25,
                         MASE_norm = do_MASE,
                         ret_series = TRUE, do_log_transform = do_log,
                         do_max_scale = do_maxnorm,
                         do_inversion = do_inverse_trans)
plot(as.numeric(res[3,-(1:3)]),type="l")
which.min(res[3,-(1:3)])

orig_coef = attr(res, "last_model")$coef



focus_series = attr(res, "forec_series")
focus_series = Filter(function (ll) grepl("Australia", ll$id),
                      focus_series)

err_aus = calc_error_summary(focus_series)


which.min(err_aus[3,-(1:3)])

plot(as.numeric(err_aus[3,-(1:3)]),type="l")



# err_aus = analyze_glob_model(focus_series, linear_model, lag_range=1:48,  MASE_norm = do_MASE,
#                    ret_series = TRUE, do_log_transform = do_log,
#                    do_max_scale = do_maxnorm,
#                    do_inversion = do_inverse_trans)
# focus_series = attr(err_aus, "forec_series")
# focus_series = Filter(function (ll) grepl("Australia", ll$id),
#                       focus_series)
# min(err_aus[3,-(1:3)])

10.3518

i = 7
best_method = 24#which.min(err_aus[3,])
x = (c(focus_series[[i]]$x, focus_series[[i]]$xx))
plot(x,type="l", main=focus_series[[i]]$id,col="red")
x = (c(focus_series[[i]]$x, focus_series[[i]]$ff[best_method,]))
lines(x,type="l",col="green")
x = (focus_series[[i]]$x)
lines(x)


###########################
###### coefficients #######
###########################

best_method = 24

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

dput(as.vector(coefficients))

