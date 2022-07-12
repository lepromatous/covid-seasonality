# Load packages ----
library(dplyr)
library(janitor)
library(prophet)
library(vroom)
library(anomalize)
library(cowplot)
library(wppExplorer)
library(prophet)
library(ggplot2)
library(tidyr)
library(RSocrata)
library(qcensus)
# yo <- c("United States", "Canada", "Austria", "Belgium", "Bulgaria", "Croatia", 
#         "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", 
#         "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", 
#         "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", 
#         "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom", 
#         "Columbia", "Brazil", "Peru", "Ecuador", "Bolivia", "Guyana", "Suriname", 
#         "Venezuela")



season <- function(){
  
  
  # pull data ----
  df <- vroom::vroom("https://covid.ourworldindata.org/data/owid-covid-data.csv")
  ## filter out non USA/CA NA sites
  df$keep <- 0
  #df$keep[df$continent=="Europe"] <- 1
  df$keep[df$continent=="North America" & df$location %in% c("United States")] <- 1
  #df$keep[df$continent=="Europe" & df$location %in% c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom")] <- 1
  #df$keep[df$continent=="South America" & df$location %in% c("Columbia", "Brazil", "Peru", "Ecuador", "Bolivia", "Guyana", "Suriname", "Venezuela")] <- 1
  #df$keep[df$location %in% countriez] <- 1
  
  # filter from above selector ----
  df %>% 
    filter(
      keep == 1
    ) -> pre
  
  locs <- unique(pre$location)
  
  # sum new cases by date ----
  pre %>%
    rename(
      cases = "new_cases"
    ) -> df
  
  # country population data ----
  ## get country codes ----
  codes <- data.frame(countrycode::countrycode(locs, origin = "country.name", destination = "iso2c"))
  codes$country <- locs
  codes <- codes[complete.cases(codes),]
  
  ## get population from wppExplorer ----
  set.wpp.year(2019)
  pops <- lapply(codes[,1], function(x) wpp.by.country(wpp.indicator("tpop"), x)) 
  lapply(pops, function(x) subset(x, x$Year == 2020)) %>%
    data.table::rbindlist() %>%
    summarise(
      pop = sum(value, na.rm=T)*1000
    ) -> pops
  
  # make rate ----
  df %>%
    mutate(
      pop = pops$pop,
      rate = cases / pop * 1000000
    ) -> df
  
  # clean and ready for modeling ----
  df %>%
    dplyr::select(
      c(date, rate, stringency_index, people_fully_vaccinated_per_hundred)
    ) %>%
    rename(
      ds = 1,
      y = 2
    ) %>%
    mutate(
      week = lubridate::ceiling_date(ds, "week", week_start = getOption("lubridate.week.start", 1))-1
    )  %>%
    group_by(week) %>%
    summarise(
      y = sum(y, na.rm=T),
      stringency_index = min(stringency_index, na.rm=T),
      people_fully_vaccinated_per_hundred = max(people_fully_vaccinated_per_hundred, na.rm=T)
    ) %>%
    filter(
      !is.nan(y)
    )%>%
    rename(
      ds = "week" ) -> df
  
  df$people_fully_vaccinated_per_hundred[df$people_fully_vaccinated_per_hundred==-Inf] <- 0
  # get regressor for uptake ----
  # uptake <- vroom::vroom("https://covid.ourworldindata.org/data/owid-covid-data.csv")
  # uptake %>%
  #   filter(
  #     location %in% locs
  #   ) %>%
  #   group_by(
  #     date
  #   ) %>%
  #   summarise(
  #     weighted_vax = mean(people_fully_vaccinated, na.rm=T)
  #   ) %>% 
  #   ungroup() %>%
  #   mutate(
  #     weighted_vax = ifelse(is.nan(weighted_vax), 0, weighted_vax)
  #   ) -> uptake
  # 
  
  # merge and remove NA to zero ----
  # df <- merge(df, uptake, by.x = "ds", by.y="date", all.x=T)
  # df$weighted_vax[is.na(df$weighted_vax)] <- 0
  
  # buffer for extra holidays ----
  
  ### subset to remove omicron
  df <- subset(df, df$ds<="2021-03-01")
  
  # Set up prophet ----
  m <- prophet(daily.seasonality= F, 
               weekly.seasonality=T,
               yearly.seasonality = T,
               interval.width = .95,
               seasonality.mode = 'additive',
               uncertainty.samples = 2000, 
               mcmc.samples=500) ##
  m <- add_regressor(m, "people_fully_vaccinated_per_hundred")
  #m <- add_regressor(m, "stringency_index")
  
  # forecast and decompose
  m <- fit.prophet(m, df) 
  
  forecast <- predict(m)
  
  # get plot prophet ----
  p<- prophet_plot_components(m, forecast, render_plot=T)
  
  return(ggsave(plot = p[[3]], filename = paste0("~/Desktop/Seasonality/", paste(locs, collapse=""), ".png")))
}


season()