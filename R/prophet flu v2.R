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


#######################################################
### read flu df 
### df scrape from:  https://apps.who.int/flumart/Default?ReportNo=12
### https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6086842/
###https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0193263
#######################################################
library(tidyverse)
library(vroom)
library(janitor)
library(countrycode)
library(stringr)
library(zoo)
library(rgeos)
library(tmap)
library(tmaptools)
#tmap_mode("view")
library(leaflet)
library(lubridate)
library(data.table)

options(warn=-1)

##########################################################################################
##########################################################################################
#### read and manage
##########################################################################################
##########################################################################################
#setwd("/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/flu_maps/app/")

df <- vroom::vroom("https://quartzpfizer.s3.amazonaws.com/who-influenza.csv")
df %>%
  janitor::clean_names() -> df
df$flupos <- df$total_number_of_influenza_positive_viruses
df$fluneg <- ifelse(is.na(df$total_number_of_influenza_negative_viruses), df$processed, df$total_number_of_influenza_negative_viruses)

df$y <- df$flupos / (df$fluneg + df$flupos) *100

df %>%
  mutate(
    moyr = zoo::as.yearmon(end_date)
  )%>%
  filter(end_date >="2009-10-01"
  ) -> df

df <- subset(df, !is.na(end_date))

df$country_area_or_territory <- stringr::str_replace(df$country_area_or_territory, " \\s*\\([^\\)]+\\)", "")
df$country_area_or_territory <- gsub(")", "", df$country_area_or_territory)

eu.countries <-  c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom of Great Britain and Northern Ireland", "United States of America")

df$ds <- df$end_date

df <- df[,c("ds", "y", "country_area_or_territory")]

library(prophet)
# Set up prophet ----
m <- prophet(daily.seasonality= F, 
             weekly.seasonality=T,
             yearly.seasonality = T,
             interval.width = .95,
             seasonality.mode = 'additive',
             uncertainty.samples = 2000, 
             mcmc.samples=2000) ##

seasonality <- function(locs, outcome = "y", titlez){
  df <- subset(df, df$country_area_or_territory == locs)
  
  df <- df[,c("ds", "y")]
  # forecast and decompose
  set.seed(1234)
  m <- fit.prophet(m, df) 
  
  forecast <- predict(m)
  
  # get plot prophet ----
  p<- prophet_plot_components(m, forecast, render_plot=T)
  
  p[[3]] +
    scale_x_datetime(expand = c(0,0), date_breaks = "1 month", date_labels = month.name) + 
    labs(
      x = "",
      y = "Yearly Seasonal Impact"
    ) + 
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.background = element_blank(),
      axis.line = element_line("black", size = 0.2),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line("gray90", 0.25)
    ) +
    annotate("rect",
             xmin = as.POSIXct("2017-01-01 GMT"), 
             xmax = as.POSIXct("2017-04-01 GMT"), 
             ymin = 0, 
             ymax = max(p[[3]]$data$yearly_upper), 
             alpha = 0.09, fill = "blue") +
    annotate("rect",
             xmin = as.POSIXct("2017-11-01 GMT"), 
             xmax = as.POSIXct("2017-12-31 GMT"), 
             ymin = 0, 
             ymax = max(p[[3]]$data$yearly_upper), 
             alpha = 0.09, fill = "blue") +
    geom_hline(yintercept = 0, linetype = 1, size = 0.5) +
    ggtitle(titlez)-> p
  
  return(p)
  #   return(ggsave(plot = p[[3]], filename = paste0("~/Desktop/Seasonality/", paste(locs, collapse=""), ".png")))
}
# 
p1.eu.flu <- seasonality(locs = eu.countries[1], outcome = "y", titlez = eu.countries[1])
p2.eu.flu <- seasonality(locs = eu.countries[2], outcome = "y", titlez = eu.countries[2])
p3.eu.flu <- seasonality(locs = eu.countries[3], outcome = "y", titlez = eu.countries[3])
p4.eu.flu <- seasonality(locs = eu.countries[4], outcome = "y", titlez = eu.countries[4])
p5.eu.flu <- seasonality(locs = eu.countries[5], outcome = "y", titlez = eu.countries[5])
p6.eu.flu <- seasonality(locs = eu.countries[6], outcome = "y", titlez = eu.countries[6])
p7.eu.flu <- seasonality(locs = eu.countries[7], outcome = "y", titlez = eu.countries[7])
p8.eu.flu <- seasonality(locs = eu.countries[8], outcome = "y", titlez = eu.countries[8])
p9.eu.flu <- seasonality(locs = eu.countries[9], outcome = "y", titlez = eu.countries[9])
p10.eu.flu <- seasonality(locs = eu.countries[10], outcome = "y", titlez = eu.countries[10])
p11.eu.flu <- seasonality(locs = eu.countries[11], outcome = "y", titlez = eu.countries[11])
p12.eu.flu <- seasonality(locs = eu.countries[12], outcome = "y", titlez = eu.countries[12])
p13.eu.flu <- seasonality(locs = eu.countries[13], outcome = "y", titlez = eu.countries[13])
p14.eu.flu <- seasonality(locs = eu.countries[14], outcome = "y", titlez = eu.countries[14])
p15.eu.flu <- seasonality(locs = eu.countries[15], outcome = "y", titlez = eu.countries[15])
p16.eu.flu <- seasonality(locs = eu.countries[16], outcome = "y", titlez = eu.countries[16])
p17.eu.flu <- seasonality(locs = eu.countries[17], outcome = "y", titlez = eu.countries[17])
p18.eu.flu <- seasonality(locs = eu.countries[18], outcome = "y", titlez = eu.countries[18])
p19.eu.flu <- seasonality(locs = eu.countries[19], outcome = "y", titlez = eu.countries[19])
p20.eu.flu <- seasonality(locs = eu.countries[20], outcome = "y", titlez = eu.countries[20])
p21.eu.flu <- seasonality(locs = eu.countries[21], outcome = "y", titlez = eu.countries[21])
p22.eu.flu <- seasonality(locs = eu.countries[22], outcome = "y", titlez = eu.countries[22])
p23.eu.flu <- seasonality(locs = eu.countries[23], outcome = "y", titlez = eu.countries[23])
p24.eu.flu <- seasonality(locs = eu.countries[24], outcome = "y", titlez = eu.countries[24])
p25.eu.flu <- seasonality(locs = eu.countries[25], outcome = "y", titlez = eu.countries[25])
p26.eu.flu <- seasonality(locs = eu.countries[26], outcome = "y", titlez = eu.countries[26])
p27.eu.flu <- seasonality(locs = eu.countries[27], outcome = "y", titlez = eu.countries[27])
p28.eu.flu <- seasonality(locs = eu.countries[28], outcome = "y", titlez = eu.countries[28])

p1.us.flu <- seasonality(locs = "United States of America", outcome = "y", titlez = "USA")



















