###########################################################################
######## create a curated total confirmed cases list of time series #######
###### not too short, not very weird after plotting
###### not low cases counts
###########################################################################



curated_cases_list = function(covid_global, MIN_MAX_NUM_CASES=500, TRAILING_MIN_COUNT_CASES = 5,
                              FINAL_MIN_LENGTH=10) {

  covid_global[is.na(covid_global)] = 0

  #remove faulty timeseries, the Unassigned from the Johns Hopskins
  covid_global = covid_global %>% filter( !grepl("Unassigned", region))

  #force Australia to appear, it is of interest and seems reliable
  entries_australia = covid_global$region[(grep("Australia|Tasmania", covid_global$region))]
  entries_australia = unique(entries_australia)


  # Select only those regions with large amount of cases

  regions_large_cases = covid_global %>% group_by(region) %>% mutate(max_cases=max(cases)) %>%
    filter(max_cases > MIN_MAX_NUM_CASES | region %in% entries_australia)
  regions_large_cases = unique(regions_large_cases$region)

  covid_global = covid_global %>% filter(region %in% regions_large_cases)



  #remove cases with few observations over 5 cases, aka weird jumps
  region_longtime = covid_global %>% group_by(region) %>% summarize(num_over = sum(cases>5))
  region_longtime = region_longtime[region_longtime$num_over > 10,]$region
  covid_global = covid_global %>% filter(region %in% region_longtime | region %in% entries_australia)


  #turn them into a list of timeseries

  cases_list = covid_global %>% select(region, date, cases) %>% group_by(region) %>%
    group_map( function(tbl, y) {
      tbl = tbl %>% arrange(date)
      list(x=tbl$cases,
           id=y$region,
           lastdate=tail(tbl$date,1))
    }
    )


  #remove trailing values less than min_count_cases
  #create problem


  cases_list = lapply(cases_list, function(ll) {
    last_val = min(which(ll$x > TRAILING_MIN_COUNT_CASES))
    if (last_val > 1) {
      ll$x = ll$x[-(1:(last_val-1))]
    }
    ll
  })



  #remove very short series
  cases_list = Filter( function(ll) length(ll$x) > FINAL_MIN_LENGTH, cases_list)

  cases_list

}


prepare_forecasts <- function(series_list, horiz=4, epidem_window_days=5, FINAL_MIN_LENGTH=10) {

  series_list = lapply(series_list, function(ll) {
    ll$xx = tail(ll$x, horiz)
    ll$x = head(ll$x, -horiz)
    ll
  })

  #remove very short series
  series_list = Filter( function(ll) length(ll$x) > FINAL_MIN_LENGTH, series_list)

  #add forecasts of the baseline models
  if (1) {
  series_list = lapply(series_list, function (ll) {
    arima_forec =  forecast::forecast( forecast::auto.arima(ll$x), h=horiz)$mean
    ets_forec = forecast::forecast( forecast::ets(ll$x), h=horiz)$mean
    theta_forec = forecast::thetaf(ll$x,horiz)$mean

    # epidemiological not ready for differenced data
    # last7 = log(tail(ll$x, epidem_window_days))
    # days = 1:epidem_window_days
    # modelm = lm(last7~days)
    # predays = (epidem_window_days+1):(epidem_window_days+horiz)
    # predays = cbind(1, predays)
    # epide_ff = as.vector(exp(predays %*% modelm$coefficients))

    ll$ff = rbind(ll$ff, arima_forec, ets_forec, theta_forec)#, epide_ff)
    ll
  })}

  series_list
}




