# https://github.com/beoutbreakprepared/nCoV2019
# https://twitter.com/COVID_Australia
# https://www.gov.uk/government/publications/covid-19-track-coronavirus-cases


library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(downloader)

################################################################
####################### SPAIN all variables ####################
################################################################

get_data_spain <- function() {
  sp_csv = read_csv("https://covid19.isciii.es/resources/serie_historica_acumulados.csv")
  sp_csv = head(sp_csv, -39)
  stopifnot( !anyNA(tail(sp_csv)))
  sp_csv <- sp_csv %>% transmute(region = `CCAA`,
                                 date = FECHA,
                                 cases = CASOS,
                                 hospital = Hospitalizados,
                                 icu = UCI,
                                 deaths = Fallecidos,
                                 recovered = Recuperados)
  sp_csv <- mutate(sp_csv, date = as_date(as_datetime(date, format = "%d/%m/%y")) )

  sp_csv
}



###############################################################
##################  ITALY ALL VARIABLES #######################
###############################################################

get_data_italy <- function() {
  ita_csv = read_csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
  ita_csv = transmute(ita_csv,
                      region=denominazione_regione,
                      date=as_date(data),
                      cases=totale_casi,
                      hospital=totale_ospedalizzati,
                      icu=terapia_intensiva,
                      deaths=deceduti,
                      recovered=dimessi_guariti)
  ita_csv
}




##############################################################
######################### UK Cases ###########################
# https://github.com/tomwhite/covid-19-uk-data
# https://coronavirus.data.gov.uk/

get_data_uk <- function() {
  uk_csv = read_csv( "https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-cases-uk.csv")
  uk_csv$TotalCases <- as.numeric(uk_csv$TotalCases)
  uk_csv$TotalCases[is.na(uk_csv$TotalCases)] <- 2

  countries_uk_csv = uk_csv %>% group_by(Country, Date) %>% summarise(TotalCases = sum(TotalCases))
  uk_csv = bind_rows(uk_csv, countries_uk_csv)

  uk_csv = uk_csv %>% transmute(region = paste(Area, Country), date=Date, cases = as.numeric(TotalCases))
  uk_csv
}



###############################################################################
############## USA CASES FROM JOHNS HOPKINS ###################################
###############################################################################

get_data_usa <- function() {
  usa_csv <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

  usa_csv = usa_csv %>% select(-(1:10))
  usa_csv = usa_csv %>% pivot_longer(cols=-1, values_to="cases")
  usa_csv = rename(usa_csv, region=Combined_Key, date=name)
  usa_csv <- mutate(usa_csv, date = as_date(as_datetime(date, format = "%m/%d/%y")) )
  usa_csv
}






###############################################################################
############## GLOBAL CASES FROM JOHNS HOPKINS ################################
###############################################################################

get_data_other <- function() {

  other_csv <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  other_csv = other_csv %>% select(-(3:4)) %>%
    mutate(region=paste(`Province/State`, `Country/Region`)) %>% select(-(1:2))

  other_csv = other_csv %>% pivot_longer(cols=-ncol(other_csv), values_to="cases")
  other_csv = rename(other_csv, date=name)
  other_csv <- mutate(other_csv, date = as_date(as_datetime(date, format = "%m/%d/%y")) )
  other_csv
}

acquire_covid_data <- function() {
  stop("acquire_covid_data function is deprecated due to data sources changing format")
 today <- lubridate::today()

  sp_csv = get_data_spain()
  ita_csv = get_data_italy()
  uk_csv = get_data_uk()
  usa_csv = get_data_usa()
  other_csv = get_data_other()

  filenames = c("covid_sp", "covid_ita",
                       "covid_uk", "covid_usa", "covid_other")

  filenames = sapply(filenames, function(name) {
    filename = paste(name, "_", today, ".csv", sep="", collapse="")
  })

  write_csv(sp_csv, filenames[[1]])
  write_csv(ita_csv, filenames[[2]])
  write_csv(uk_csv, filenames[[3]])
  write_csv(usa_csv, filenames[[4]])
  write_csv(other_csv, filenames[[5]])

  bind_rows(sp_csv, ita_csv, uk_csv,
            usa_csv, other_csv)
}


#plot the list of cases for debuggin purposes
plot_list_cases <- function(cases_list) {
  for (i in 1:length(cases_list)) {
    plot(c(cases_list[[i]]$x, cases_list[[i]]$xx), type="b",
         main = paste("Cases", cases_list[[i]]$id))
    readline()
  }
}






########################## from guidotti package #########
#https://github.com/emanuele-guidotti/COVID19

# library("COVID19")
#
#
# guido_FRA = covid19("FRA", level = 2) %>% select(id, date, confirmed) %>%
#             rename(region=id, cases=confirmed)
#
# other_csv <- bind_rows(other_csv, guido_FRA)
# write_csv(other_csv, "covid_other.csv")

#
#
# country = read_csv("https://storage.guidotti.dev/covid19/data-1.csv")
#
# a = country %>% group_by(id) %>% summarize(maxn = max(confirmed))
#
#
# state = read_csv("https://storage.guidotti.dev/covid19/data-2.csv")
#
# a = state %>% group_by(id) %>% summarize(maxn = max(confirmed))
#
# city = read_csv("https://storage.guidotti.dev/covid19/data-3.csv")
#
#
# a = city %>% group_by(id) %>% summarize(maxn = max(confirmed))
#


