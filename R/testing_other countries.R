# Load packages ----
library(tidyverse)
library(janitor)
library(prophet)
library(vroom)
library(anomalize)
library(cowplot)

# get populations
# get character codes for countries
data(iso3166) 
iso3166 %>%
  dplyr::select(name, charcode) -> iso_codes
rm(iso3166)
rm(iso3166ud)
gc()

# OWID Data
df <- vroom::vroom("https://covid.ourworldindata.org/data/owid-covid-data.csv")

country.options <- unique(df$location[df$location %in% iso_codes$name])

plotter <- function(locales = NULL, decomposition.method = "twitter", anomalize.method = "gesd"){

  df %>%
    filter(
      location %in% locales
    ) %>%
    dplyr::select(
      c(date, new_cases)
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
      ds = "week"
    ) %>%
    filter(
      ds <= "2022-06-20"
    ) -> df
  
  
  # get population
  set.wpp.year(2019)
 
  pops <- lapply(locales, function(x) wpp.by.country(wpp.indicator("tpop"), iso_codes$charcode[iso_codes$name %in% x])) 
  lapply(pops, function(x) subset(x, x$Year == 2020))  %>%
    data.table::rbindlist() %>%
    summarise(
      pop = sum(value, na.rm=T)*1000
    ) -> pops

    df %>%
    mutate(
      pop = pops$pop
      ) -> df
  
  df$y <- df$y / df$pop *1000000

  # build prophet data ----
  df.prophet <- data.frame(y=df[,"y"], ds=df$ds, date=df$ds)
  df.prophet <- column_to_rownames(df.prophet, var = "ds")
  names(df.prophet)<-c("y", "ds")
  
  df.prophet$y[df.prophet$y==0] <- NA
  
  df.prophet$y <- imputeTS::na_ma(df.prophet$y, k=1, weighting = "simple")

  #recomposed version
  df.prophet%>%
    tibble() %>%
    time_decompose(y, method = decomposition.method, frequency = "auto", trend = "auto") %>%
    anomalize(remainder, method = anomalize.method, alpha = 0.05, max_anoms = 0.3) %>%
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
      limits = c(-1200, (max(yo$observed) + (5000 - max(yo$observed) %% 5000))), breaks = c(seq(0, (max(yo$observed) + (5000 - max(yo$recomposed_l2) %% 1000)), by=5000))
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
  
  return(observed_recomposed)
  
}

plotter(locales = "Portugal", decomposition.method = "twitter", anomalize.method = "gesd")

country.options
