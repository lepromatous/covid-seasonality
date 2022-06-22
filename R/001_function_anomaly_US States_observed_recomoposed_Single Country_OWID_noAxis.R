# Load packages ----
library(tidyverse)
library(janitor)
library(prophet)
library(vroom)
library(anomalize)

library(tidyquant)
library(plotly)

jhu <- vroom::vroom("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
#arrow::write_feather(jhu, "/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/covid-seasonality/Data/jhu.feather")

jhu <- jhu[,c(7, 12:ncol(jhu))]

jhu %>%
  pivot_longer(
    data = ., cols = 2:ncol(jhu),
    names_to = "week",
    values_to = "count"
  ) %>%
  mutate(
    date = as.Date(week, format="%m/%d/%y")
  ) %>%
  janitor::clean_names() %>%
  group_by(province_state, date) %>%
  summarise(
    cases = sum(count, na.rm=T)
  ) %>%
  ungroup() %>%
  group_by(province_state) %>%
  arrange(date) %>%
  mutate(
    new_cases = cases - lag(cases)
  ) %>%
  filter(
    province_state %in% state.name
  ) %>%
  ungroup()-> df

regions <- vroom::vroom("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")

# library(tidycensus)
# get_estimates(geography="region", product = "population")
# https://www.census.gov/popclock/data_tables.php?component=growth  --- for 2021 data

grr_us <- function(locale, by.tick=5000){
  
  if(locale == "South"){
    keeps <- regions$State[regions$Region=="South"]
  } else if(locale == "Midwest"){
    keeps <- regions$State[regions$Region=="Midwest"]
  } else if(locale == "Northeast"){
    keeps <- regions$State[regions$Region=="Northeast"]
  } else if(locale == "West"){
    keeps <- regions$State[regions$Region=="West"]
  }
  
  df %>%
    filter(
      province_state %in% keeps
    ) %>%
    group_by(date) %>%
    summarise(
      new_cases2 = sum(new_cases, na.rm=T)
    ) -> df
  
  df %>%
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
      ds = "week"
    ) %>%
    filter(
      ds <= "2022-04-09"
    ) -> df
  
  # pops from https://www.worldometers.info/world-population/south-korea-population/ Apr 29/30, 2022
  df$pop <- ifelse(locale == "South", 127225329,
                   ifelse(locale == "Northeast", 57159838,
                          ifelse(locale == "Midwest", 68841444,
                                 ifelse(locale == "West", 78667134, NA))))
  
  
  
  #pop <- 65537574 + 84274792 + 68542352 + 60300591 + 46787755
  
  df$y <- df$y / df$pop *1000000
  # france 65537574
  # germany 84274792
  # uk 68542352
  # italy 60300591
  # spain 46787755
  #858342 5/1/2022
  df <- subset(df, df$ds<="2022-04-09")
  
  
  # build prophet data ----
  df.prophet <- data.frame(y=df[,"y"], ds=df$ds, date=df$ds)
  df.prophet <- column_to_rownames(df.prophet, var = "ds")
  names(df.prophet)<-c("y", "ds")
  
  df.prophet$y[df.prophet$y==0] <- NA
  
  df.prophet$y <- imputeTS::na_ma(df.prophet$y, k=1, weighting = "simple")
  #df.prophet$y <- scale(df.prophet$y)
  #df.prophet <- subset(df.prophet, df.prophet$ds<="2022-01-01")
  #recomposed version
  df.prophet%>%
    tibble() %>%
    time_decompose(y, method = "twitter", frequency = "auto", trend = "auto") %>%
    anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.3) %>%
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
      ymin = -1200,
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
    scale_color_manual(name = "Anomaly", values = c("black" = "#272936", "red" = "#88B0AD"), labels = c("No", "Yes")) +
    geom_rect(data = label_range, fill = "#A66923", color = "#f2f2f2", alpha = 0.5,
              aes(xmin = xmin, xmax = xmax, 
                  ymin = ymin, ymax = ymax-200,
                  group = year)) +
    # geom_text(data = label_range,
    #           aes(x = xmin+25, y = ymin+500,
    #               group = year, label = year)) +
    geom_ribbon(
      aes(x= ds, ymin = recomposed_l1, ymax=recomposed_l2), color = "#272936", alpha=0.2
    ) +
    scale_x_date(
      date_breaks = "1 month", 
      date_labels = "%b", 
      expand=c(0.01, 0.0)) +
    scale_y_continuous(
      limits = c(-1200, (max(yo$observed) + (5000 - max(yo$observed) %% 5000))), breaks = c(seq(0, (max(yo$observed) + (5000 - max(yo$recomposed_l2) %% 1000)), by=by.tick))
    ) + 
    labs(
      colour = "Anomaly",
      x="",
      y="Rate of COVID-19 per 1,000,000 Population\n"
    ) + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line("black", size=0.2),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y =element_blank(),
      panel.grid.major.x = element_line("gray90", 0.05),
      legend.position = c(0.5,0.9),
      legend.direction = "horizontal",
    ) -> observed_recomposed
  return(observed_recomposed)
}

# grr_us("Midwest", by.tick=3000)
# ggsave(paste0("~/Desktop/US_anomaly_Midwest_noaxis.pdf"), width=10, height=6)

grr_us("Northeast")
ggsave("~/Desktop/Figure_2A_US_anomaly_Northeast.pdf", width=10, height=6)

grr_us("West")
ggsave("~/Desktop/Figure_2C_US_anomaly_West.pdf", width=10, height=6)

# grr_us("South")
# ggsave(paste0("~/Desktop/US_anomaly_South_noaxis.pdf"), width=10, height=6)

