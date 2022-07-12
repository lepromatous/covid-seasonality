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



# pull data ----
df <- vroom::vroom("https://covid.ourworldindata.org/data/owid-covid-data.csv")
# pull hemi lookup ---
hemi <- vroom::vroom("https://raw.githubusercontent.com/lepromatous/covid-seasonality/main/hemi.csv", delim = ",")
## merge ----
df <- merge(df, hemi, by.x = "iso_code", by.y = "iso_a3", all.x=T)

## filter out non USA/CA NA sites
df$keep <- 0
#df$keep[df$continent=="Europe"] <- 1
df$keep[df$continent=="North America" & df$location %in% c("United States", "Canada")] <- 1
#df$keep[df$continent=="Europe" & df$location %in% c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom")] <- 1
#df$keep[df$continent=="South America" & df$location %in% c("Columbia", "Brazil", "Peru", "Ecuador", "Bolivia", "Guyana", "Suriname", "Venezuela")] <- 1
#df$keep[df$continent=="South America" & df$location %in% c("Chile", "Argentina")] <- 1

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
covariants <- vroom::vroom("https://s3.amazonaws.com/quartzdata/datasets/covariants.csv")
covariants %>%
  filter(
    state=="USA"
  ) %>%
  janitor::clean_names() %>%
  rowwise() %>%
  mutate(
    total = sum(c_across(where(is.numeric)), na.rm=T)
  ) %>%
  relocate(
    week, .before = x20b_s_732a
  ) %>%
  dplyr::select(
    -state
  ) %>%
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
    cols = 1:32, values_to = "freq", names_to="variant"
  ) -> plot

plot %>%
  group_by(week) %>%
  filter(freq == max(freq)) -> plot

wildtype <- data_frame(
  variant = 'wildtype',
  ds = seq.Date(from=as.Date(min(df$ds)), to = as.Date("2021-04-04"), by="day")
)
delta <- data_frame(
  variant = 'delta',
  ds = seq.Date(from=as.Date("2021-04-05"), to = as.Date("2021-12-13"), by="day")
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
urlz <- "https://data.cdc.gov/resource/gxj9-t96f.json"

tokenz<-'chCxsk4zel6QXbaemotF65C9L'

read.socrata(
  urlz,
  app_token = tokenz,
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!"
) %>%
  tibble() -> uptake

agez<- as.character(unique(uptake$agegroupvacc))
uptake$pop <- NA
uptake$pop[uptake$agegroupvacc == agez[1]] <- qcensus::qcensus(2022, c(12:17))
uptake$pop[uptake$agegroupvacc == agez[2]] <- qcensus::qcensus(2022, c(18:24))
uptake$pop[uptake$agegroupvacc == agez[3]] <- qcensus::qcensus(2022, c(25:39))
uptake$pop[uptake$agegroupvacc == agez[4]] <- qcensus::qcensus(2022, c(40:49))
uptake$pop[uptake$agegroupvacc == agez[5]] <- qcensus::qcensus(2022, c(5:11))
uptake$pop[uptake$agegroupvacc == agez[6]] <- qcensus::qcensus(2022, c(50:64))
uptake$pop[uptake$agegroupvacc == agez[7]] <- qcensus::qcensus(2022, c(65:74))
uptake$pop[uptake$agegroupvacc == agez[8]] <- qcensus::qcensus(2022, c(75:100))

# weighted mean by age ----
uptake %>%
  group_by(cdc_case_earliest_dt) %>%
  summarise(
    weighted_vax = weighted.mean(as.numeric(series_complete_pop_pct), pop)
  )  -> uptake


# merge and remove NA to zero ----
df.prophet2 <- merge(df.prophet2, uptake, by.x = "ds", by.y="cdc_case_earliest_dt", all.x=T)
df.prophet2$weighted_vax[is.na(df.prophet2$weighted_vax)] <- 0

# buffer for extra holidays ----
source("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/covid-seasonality/R/Old/old CID/prophet extra holidays.R")

df.prophet2$delta <- ifelse(df.prophet2$variant==2,1,0)
df.prophet2$omicron <- ifelse(df.prophet2$variant==3,1,0)

### subset to remove omicron
#df.prophet2 <- subset(df.prophet2, df.prophet2$ds<="2021-12-18")

# Set up prophet ----
m <- prophet(daily.seasonality= F, 
             weekly.seasonality=T,
             yearly.seasonality = T,
             interval.width = .95,
             seasonality.mode = 'additive',
             uncertainty.samples = 4000, 
             mcmc.samples=1000,
             holidays = holidays_extra) ##
# add us holidays ----
m <- add_country_holidays(m, country_name = 'US')
m <- add_regressor(m, "delta")
m <- add_regressor(m, "omicron")
m <- add_regressor(m, "weighted_vax")

# forecast and decompose
m <- fit.prophet(m, df.prophet2) 
#future <- make_future_dataframe(m)

forecast <- predict(m)

# get plot prophet ----
p<- prophet_plot_components(m, forecast, render_plot=T)
p
# plot_components(m, forecast)
# 
# 
# # https://robjhyndman.com/hyndsight/detecting-seasonality/
# #  If the hypothesis test is significant, we can conclude that the data are very unlikely to have been generated from the simpler (non-seasonal) model
# library(forecast)
# 
# fit1 <- ets(Yt) # not seasonal
# fit2 <- ets(df.prophet2$y, model="ANN")
# 
# deviance <- 2*c(logLik(fit1) - logLik(fit2))
# df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df
# #P value
# 1-pchisq(deviance,df) # non significnat seasonal component
# 
# #install.packages("/Users/timothywiemken/Downloads/greenbrown_2.4.3.tar.gz", repos = NULL, type="source")
# library(greenbrown)
# # https://greenbrown.r-forge.r-project.org/man/Seasonality.html
# Yt <- ts(df.prophet2$y, start = c(2020, 01), frequency = 52)
# Seasonality(Yt)
# plot(Yt)
# 
# plot(decompose(Yt))
