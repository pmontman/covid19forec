


# Join together to form a big dataset
# covid_global = read_csv("covid_sp.csv")
# covid_global = bind_rows(covid_global, read_csv("covid_ita.csv"))
# covid_global = bind_rows(covid_global, read_csv("covid_uk.csv"))
# covid_global = bind_rows(covid_global, read_csv("covid_usa.csv"))
# covid_global = bind_rows(covid_global, read_csv("covid_other.csv"))


#######


covid_global = acquire_covid_data()


cases_list = curated_cases_list(covid_global)
cases_list = prepare_forecasts(cases_list, horiz = 5)


do_MASE = FALSE
do_log = TRUE
do_maxnorm = FALSE


res = analyze_glob_model(cases_list, linear_model,  MASE_norm = do_MASE,
                         ret_series = TRUE, do_log_transform = do_log,
                         do_max_scale = do_maxnorm)
res


ff_series = attr(res, "forec_series")
aust_series = Filter(function (ll) grepl("Australia", ll$id),
                                        ff_series)

res = calc_error_summary(aust_series)
rel_smape = as.numeric(res[2,])
rel_smape = rel_smape / mean(sort(rel_smape)[1:10])
rel_abs =  as.numeric(res[3,])
rel_abs = rel_abs / mean(sort(rel_abs)[1:10])
rel_mase =  as.numeric(res[1,])
rel_mase = rel_mase / mean(sort(rel_mase)[1:10])

rel_mase + rel_smapes + rel_abs

which.min(rel_mase + rel_smape + rel_abs)
best_method = which.min(rel_mase + rel_smape + rel_abs) - 4

rs =  sample(1:length(aust_series),1)
plot( (c(aust_series[[rs]]$x, aust_series[[rs]]$ff[best_method,])),type="b", col="red", lwd=2,
      main = paste("cases in ", aust_series[[rs]]$id),
      ylab = " patients", xlab="Days since 02 February")
lines( (c(aust_series[[rs]]$x, aust_series[[rs]]$xx)),type="b", lwd=3)


##################################################################

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


ff_series = attr(res, "forec_series")

aust_series = Filter(function (ll) grepl("Australia", ll$id),
                     ff_series)



rs =  sample(1:length(aust_series),1)
plot( (c(aust_series[[rs]]$x, aust_series[[rs]]$ff[best_method,])),type="b", col="red", lwd=2,
      main = paste("cases in ", aust_series[[rs]]$id),
      ylab = " patients", xlab="Days since 02 February")
lines( (c(aust_series[[rs]]$x, aust_series[[rs]]$xx)),type="b", lwd=3)
diff(aust_series[[rs]]$x)
diff(aust_series[[rs]]$ff[best_method,])

days_forec = lubridate::as_date("2020-04-16")
days_forec = days_forec+(1:horiz -1)

forecs = sapply(aust_series, function(ll) {
  ff = ll$ff[best_method,]
  for (i in 2:length(ff)) {
    ff[i] = max(ff[i], ff[i-1])
  }
  round(ff,2)}
  )

regions = sapply(aust_series, function(ll) {
  words = unlist(strsplit(ll$id, " ")[[1]])
  words = head(words,-1)
  paste(words, collapse=" ")
  }
  )
colnames(forecs) <- regions
rownames(forecs) <- as.character(days_forec)

forecs = as_tibble(forecs)
forecs = cbind(days_forec, forecs)
statistic="mean"
forecs = cbind(statistic, forecs)

forecs = forecs %>% pivot_longer(cols = 3:ncol(forecs), names_to="region", values_to="value")

forecs = forecs %>% rename(state="region", date = "days_forec",  confirmed="value")
forecs = forecs %>% select(state, statistic, date, confirmed)

write_csv(forecs, "covid_aust_monash-pmm_april_16.csv")

a = forecs %>% pivot_wider(values_from = "confirmed", names_from = "date")


install.packages("tidyjson")
library(tidyjson)


download.file(https://interactive.guim.co.uk/docsdata/1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE.json"")

bb = jsonlite::read_json("1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE.json", "json")

bb %>% spread_all()


glob_lin_model = attr(res, "last_model")
aust_series_15_apr = lapply(aust_series, function (ll) {
  ll$ff = ll$ff[best_method, ,drop=FALSE]
  ll$xx = NULL
  ll})

fix_decr_forecasts = function(x) {
  for (i in 2:length(x)) {
    x[i] = max(x[i], x[i-1])
  }
  round(x,2)
  }


aust_series_15_apr
save(glob_lin_model, deprecated_forec_recursive,
     fix_decr_forecasts, file="aust_globin_15_apr.RData")


fff= deprecated_forec_recursive(glob_lin_model$coefficients, rnorm(100), h=28)
fix_decr_forecasts(fff)
