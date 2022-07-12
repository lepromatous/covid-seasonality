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

# northern hem only ----
df %>% 
  filter(
    keep == 1
  ) -> pre

locs <- unique(pre$location)

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

df %>%
  mutate(
    pop = pops$pop,
    rate = cases / pop * 1000000
  ) -> df


df %>%
    select(
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
  #   ) %>%
  # filter(
  #   ds >= "2020-01-01"
  #   )-> df
  
  #df.prophet$y <- scale(df.prophet$y)
  #df.prophet <- subset(df.prophet, df.prophet$ds<="2022-01-01")
  #recomposed version
  df %>%
    tibble() %>%
    time_decompose(y, method = "twitter", frequency = "auto", trend = "auto") %>%
    anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.5) %>%
    time_recompose() %>%
    plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5) -> p_recomposed
  
  
  p_recomposed +
    labs(
      x="\n Date",
      y = "Value \n"
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand=c(0.05,0.05)) +
    scale_y_continuous(expand=c(0.2,0.1)) +
    theme(
      axis.text.x = element_text(angle=90)
    ) -> p2
  
  p2[[1]] -> yo
  
  yo$ds <- as.Date(yo$ds)
  yo$year <- lubridate::year(yo$ds)
  yo$anomaly <- ifelse(yo$observed<=yo$recomposed_l2 & yo$observed>=yo$recomposed_l1,"No","Yes")
  yo$recomposed_l1[yo$recomposed_l1<0] <- 0
  
  
  yo %>%
    group_by(year) %>%
    summarise(
      xmin = min(ds),
      xmax = max(ds),
      ymin = -1000,
      ymax = 0 ) -> label_range
  
  
  leg <-  tibble(
    colour = ifelse(yo$anomaly=="Yes", "red", "black"),
    size = ifelse(yo$anomaly=="Yes", 1.5,1),
    size2 = ifelse(yo$anomaly=="Yes", 3,1),
    shape = ifelse(yo$anomaly=="Yes", 1,19),
    size_disappear = ifelse(yo$anomaly=="Yes", 3,1),
    stroke = ifelse(yo$anomaly=="Yes", 1.5,0.8)
  )
  
  # 
  # remotes::install_github("quartzsoftwarellc/ggquartz")
  # library(ggquartz)
  
  
  
  # replot ----
  ggplot(data = yo) + 
    geom_line(
      aes(x= ds, y=observed)
    ) +
    geom_point(
      aes(x=ds, y=observed, colour = leg$colour, size = leg$size_disappear, shape = leg$shape), 
      size = leg$size_disappear,
      shape = leg$shape,
      stroke= leg$stroke
    ) +
    # geom_point(
    #   aes(x=ds, y=value, shape = leg$shape, size = leg$size2), 
    #   size = leg$size2,
    #   shape = leg$shape88B0AD
    # ) +
    scale_color_manual(name = "Anomaly", values = c("black" = "#272936", "red" = "#cc292b"), labels = c("No", "Yes")) +
    geom_rect(data = label_range, fill = "#0070BF", color = "#f2f2f2", alpha = 0.5,
              aes(xmin = xmin, xmax = xmax, 
                  ymin = ymin+150, ymax = ymax-200,
                  group = year)) +
    geom_text(data = label_range,
              aes(x = xmin+25, y = ymin+500,
                  group = year, label = year)) +
    geom_ribbon(
      aes(x= ds, ymin = recomposed_l1, ymax=recomposed_l2), color = "#272936", alpha=0.2
    ) +
    scale_x_date(
      date_breaks = "1 month", 
      date_labels = "%b", 
      expand=c(0.01, 0.0)) +
    scale_y_continuous(
      limits = c(-1000, (max(yo$observed) + (5000 - max(yo$observed) %% 5000))), breaks = c(seq(0, (max(yo$observed) + (5000 - max(yo$recomposed_l2) %% 1000)), by=1000))
    ) + 
    labs(
      colour = "Anomaly",
      x="",
      y="Rate of COVID-19 per 1,000,000 Population\n"
    ) + 
    theme(
      axis.text.x = element_text(angle=90),
      panel.background = element_blank(),
      axis.line = element_line("black", size=0.2),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y =element_blank(),
      panel.grid.major.x = element_line("gray90", 0.05),
      legend.position = "none"
    ) +
    guides(colour = guide_legend(override.aes = list(size = c(1,5),
                                                     shape = c(19,1),
                                                     stroke = c(0.8, 2)))) -> observed_recomposed
  observed_recomposed


  
#   #### WEEKLY TBATS: https://stats.stackexchange.com/questions/57705/identify-seasonality-in-time-series-data
#   library(forecast)
#   x <- ts(df$y, frequency=365/1)
#   fit <- tbats(x)
#   seasonal <- !is.null(fit$seasonal)
#   #hen seasonal will be TRUE if a seasonal model is chosen and otherwise FALSE.
#   
#   yo <- decompose(x)
# plot(yo)
# acf(x)
# 
# auto.arima(x, test = "adf")
