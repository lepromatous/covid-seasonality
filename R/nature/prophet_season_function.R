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
yo <- c("United States", "Canada", "Austria", "Belgium", "Bulgaria", "Croatia", 
        "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", 
        "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", 
        "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", 
        "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom", 
        "Columbia", "Brazil", "Peru", "Ecuador", "Bolivia", "Guyana", "Suriname", 
        "Venezuela")
        
        

season <- function(country.num = NULL){
  
  countriez <- yo[country.num]

# pull data ----
df <- vroom::vroom("https://covid.ourworldindata.org/data/owid-covid-data.csv")
# pull hemi lookup ---
hemi <- vroom::vroom("https://raw.githubusercontent.com/lepromatous/covid-seasonality/main/hemi.csv", delim = ",")
## merge ----
df <- merge(df, hemi, by.x = "iso_code", by.y = "iso_a3", all.x=T)

## filter out non USA/CA NA sites
df$keep <- 0
#df$keep[df$continent=="Europe"] <- 1
#df$keep[df$continent=="North America" & df$location %in% c("United States", "Canada")] <- 1
#df$keep[df$continent=="Europe" & df$location %in% c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom")] <- 1
#df$keep[df$continent=="South America" & df$location %in% c("Columbia", "Brazil", "Peru", "Ecuador", "Bolivia", "Guyana", "Suriname", "Venezuela")] <- 1
df$keep[df$location %in% countriez] <- 1

# filter from above selector ----
df %>% 
  filter(
    keep == 1
  ) -> pre

locs <- unique(pre$location)

# sum new cases by date ----
pre %>%
  group_by(
    date
  ) %>%
  summarise(
    cases = sum(new_cases, na.rm=T)
  ) %>% 
  ungroup()-> df

# country population data ----
## get country codes ----
codes <- data.frame(countrycode::countrycode(locs, origin = "country.name", destination = "iso2c"))
codes$country <- locs
codes <- codes[complete.cases(codes),]
holi <- codes[[1]]

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
    c(date, rate)
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
    y = sum(y, na.rm=T)
  ) %>%
  filter(
    !is.nan(y)
  )%>%
  rename(
    ds = "week" ) -> df

# add covariants as regressor ----
covariants <- tidyiddr::covariants_country(use_disk = F)

covariants %>%
  filter(
    country%in% countriez
  ) %>%
  janitor::clean_names() %>%
  rowwise() %>%
  mutate(
    total = sum(c_across(where(is.numeric)), na.rm=T)
  ) %>%
  relocate(
    week, .before = x20e_eu1
  ) %>%
  dplyr::select(
    -country
  ) %>%
  ungroup() %>%
  group_by(
    week
  ) %>%
  summarise_at(
    vars(-group_cols()), sum, na.rm=T
  ) %>%
  ungroup() %>%
  pivot_longer(cols = contains("_"), names_to = "variant", values_to = "vals") %>%
  mutate(
    vals= vals /total *100
  ) %>%
  pivot_wider(
    names_from = "variant", values_from = "vals"
  ) %>%
  dplyr::select(
    -total
  ) %>%
  mutate_all(
    ~ifelse(is.nan(.), 0, .)
  ) %>%
  replace(
    is.na(.), 0
  )-> covariants

week <- covariants$week

# get most common variant of that week ----
covariants %>%
  dplyr::select(-week) %>%
  rowwise() %>%
  mutate(variant.name = names(cur_data())[which.max(c_across(everything()))]) -> covariants
covariants$week <- as.Date(week, origin="1970-01-01")

covariants %>%
  pivot_longer(
    cols = 1:(ncol(covariants)-2), values_to = "freq", names_to="variant"
  ) -> plot

plot %>%
  group_by(week) %>%
  filter(freq == max(freq)) -> plot

wildtype <- data_frame(
  variant = 'wildtype',
  ds = seq.Date(from=as.Date(min(df$ds)), to = as.Date("2021-09-05"), by="day")
)
delta <- data_frame(
  variant = 'delta',
  ds = seq.Date(from=as.Date("2021-09-06"), to = as.Date("2021-12-13"), by="day")
)
omicron <- data_frame(
  variant = 'omicron',
  ds = seq.Date(from=as.Date("2021-12-14"), to = as.Date(max(df$ds)), by="day")
)
variants <- bind_rows(wildtype, delta, omicron)

# merge variant regressor and data ----
df.prophet2 <- merge(df, variants, by="ds", all.x=T)
df.prophet2$variant <- ifelse(df.prophet2$variant=="wildtype", 1,
                              ifelse(df.prophet2$variant == "delta", 2, 3))


# get regressor for uptake ----
uptake <- vroom::vroom("https://covid.ourworldindata.org/data/owid-covid-data.csv")
uptake %>%
  filter(
    location %in% countriez
  ) %>%
  group_by(
    date
  ) %>%
  summarise(
    weighted_vax = mean(people_fully_vaccinated, na.rm=T)
  ) %>% 
  ungroup() %>%
  mutate(
    weighted_vax = ifelse(is.nan(weighted_vax), 0, weighted_vax)
  ) -> uptake

names(uptake)

# merge and remove NA to zero ----
df.prophet2 <- merge(df.prophet2, uptake, by.x = "ds", by.y="date", all.x=T)
df.prophet2$weighted_vax[is.na(df.prophet2$weighted_vax)] <- 0

# buffer for extra holidays ----
#source("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/covid-seasonality/R/Old/old CID/prophet extra holidays.R")

df.prophet2$delta <- ifelse(df.prophet2$variant==2,1,0)
df.prophet2$omicron <- ifelse(df.prophet2$variant==3,1,0)

### subset to remove omicron
df.prophet2 <- subset(df.prophet2, df.prophet2$ds<="2021-12-18")

# Set up prophet ----
m <- prophet(daily.seasonality= F, 
             weekly.seasonality=T,
             yearly.seasonality = T,
             interval.width = .95,
             seasonality.mode = 'additive',
             uncertainty.samples = 2000, 
             mcmc.samples=500) ##
# add us holidays ----
# https://github.com/dr-prodigy/python-holidays
m <- add_country_holidays(m, country_name = 'BR')
m <- add_regressor(m, "delta")
m <- add_regressor(m, "omicron")
m <- add_regressor(m, "weighted_vax")
#https://www.kaggle.com/code/vbmokin/covid-19-in-canada-prophet-with-holidays-tuning/notebook
# forecast and decompose
m <- fit.prophet(m, df.prophet2) 
#future <- make_future_dataframe(m)

forecast <- predict(m)

# get plot prophet ----
p<- prophet_plot_components(m, forecast, render_plot=T)

return(ggsave(plot = p[[4]], filename = paste0("~/Desktop/Seasonality/", countriez, ".png")))
}


sapply(1:length(yo), function(x) season(x))
