# Load packages ----
library(pacman)
pacman::p_load(tidyverse, 
              janitor, 
              prophet, 
              vroom, 
              anomalize, 
              cowplot, 
              wppExplorer, 
              prophet, 
              ggplot2, 
              tidyr, 
              RSocrata, 
              qcensus)

eu.countries <-  c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom")

# pull data ----
df <- vroom::vroom("https://covid.ourworldindata.org/data/owid-covid-data.csv")

df <- subset(df, df$date>"2020-02-28" & df$date <= "2022-12-31")

# Set up prophet ----
m <- prophet(daily.seasonality= F, 
             weekly.seasonality=T,
             yearly.seasonality = T,
             interval.width = .95,
             seasonality.mode = 'additive',
             uncertainty.samples = 2000, 
             mcmc.samples=2000) ##
m <- add_regressor(m, "stringency_index")
  
seasonality <- function(locs, outcome, titlez){
  df <- subset(df, df$location == locs)
  
  df <- df[,c("date", outcome, "stringency_index")]
  names(df) <- c("ds", "y", "stringency_index")
  df <- subset(df, !is.na(df$y))
  df$stringency_index <- zoo::na.locf.default(df$stringency_index)
  
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
    ggtitle(titlez)-> p.fin
  
  return(list(p, p.fin))
#   return(ggsave(plot = p[[3]], filename = paste0("~/Desktop/Seasonality/", paste(locs, collapse=""), ".png")))
}

# 
p1.eu.cases <- seasonality(locs = eu.countries[1], outcome = "new_cases_per_million", titlez = eu.countries[1])
p2.eu.cases <- seasonality(locs = eu.countries[2], outcome = "new_cases_per_million", titlez = eu.countries[2])
p3.eu.cases <- seasonality(locs = eu.countries[3], outcome = "new_cases_per_million", titlez = eu.countries[3])
p4.eu.cases <- seasonality(locs = eu.countries[4], outcome = "new_cases_per_million", titlez = eu.countries[4])
p5.eu.cases <- seasonality(locs = eu.countries[5], outcome = "new_cases_per_million", titlez = eu.countries[5])
p6.eu.cases <- seasonality(locs = eu.countries[6], outcome = "new_cases_per_million", titlez = eu.countries[6])
p7.eu.cases <- seasonality(locs = eu.countries[7], outcome = "new_cases_per_million", titlez = eu.countries[7])
p8.eu.cases <- seasonality(locs = eu.countries[8], outcome = "new_cases_per_million", titlez = eu.countries[8])
p9.eu.cases <- seasonality(locs = eu.countries[9], outcome = "new_cases_per_million", titlez = eu.countries[9])
p10.eu.cases <- seasonality(locs = eu.countries[10], outcome = "new_cases_per_million", titlez = eu.countries[10])
p11.eu.cases <- seasonality(locs = eu.countries[11], outcome = "new_cases_per_million", titlez = eu.countries[11])
p12.eu.cases <- seasonality(locs = eu.countries[12], outcome = "new_cases_per_million", titlez = eu.countries[12])
p13.eu.cases <- seasonality(locs = eu.countries[13], outcome = "new_cases_per_million", titlez = eu.countries[13])
p14.eu.cases <- seasonality(locs = eu.countries[14], outcome = "new_cases_per_million", titlez = eu.countries[14])
p15.eu.cases <- seasonality(locs = eu.countries[15], outcome = "new_cases_per_million", titlez = eu.countries[15])
p16.eu.cases <- seasonality(locs = eu.countries[16], outcome = "new_cases_per_million", titlez = eu.countries[16])
p17.eu.cases <- seasonality(locs = eu.countries[17], outcome = "new_cases_per_million", titlez = eu.countries[17])
p18.eu.cases <- seasonality(locs = eu.countries[18], outcome = "new_cases_per_million", titlez = eu.countries[18])
p19.eu.cases <- seasonality(locs = eu.countries[19], outcome = "new_cases_per_million", titlez = eu.countries[19])
p20.eu.cases <- seasonality(locs = eu.countries[20], outcome = "new_cases_per_million", titlez = eu.countries[20])
p21.eu.cases <- seasonality(locs = eu.countries[21], outcome = "new_cases_per_million", titlez = eu.countries[21])
p22.eu.cases <- seasonality(locs = eu.countries[22], outcome = "new_cases_per_million", titlez = eu.countries[22])
p23.eu.cases <- seasonality(locs = eu.countries[23], outcome = "new_cases_per_million", titlez = eu.countries[23])
p24.eu.cases <- seasonality(locs = eu.countries[24], outcome = "new_cases_per_million", titlez = eu.countries[24])
p25.eu.cases <- seasonality(locs = eu.countries[25], outcome = "new_cases_per_million", titlez = eu.countries[25])
p26.eu.cases <- seasonality(locs = eu.countries[26], outcome = "new_cases_per_million", titlez = eu.countries[26])
p27.eu.cases <- seasonality(locs = eu.countries[27], outcome = "new_cases_per_million", titlez = eu.countries[27])
p28.eu.cases <- seasonality(locs = eu.countries[28], outcome = "new_cases_per_million", titlez = eu.countries[28])

## usa canada
p1.us.cases <- seasonality(locs = "United States", outcome = "new_cases_per_million", titlez = "USA")


############################################################
### Make TREND PLOTS ----
library(gridExtra)
library(grid)
library(gtable)
listz.trend <- c(
  p1.eu.cases[[1]][1],p2.eu.cases[[1]][1],p3.eu.cases[[1]][1],p4.eu.cases[[1]][1],
  p5.eu.cases[[1]][1],p6.eu.cases[[1]][1],p7.eu.cases[[1]][1],p8.eu.cases[[1]][1],
  p9.eu.cases[[1]][1],p10.eu.cases[[1]][1],p11.eu.cases[[1]][1],p12.eu.cases[[1]][1],
  p13.eu.cases[[1]][1],p14.eu.cases[[1]][1],p15.eu.cases[[1]][1],p16.eu.cases[[1]][1],
  p17.eu.cases[[1]][1],p18.eu.cases[[1]][1],p19.eu.cases[[1]][1],p20.eu.cases[[1]][1],
  p21.eu.cases[[1]][1],p22.eu.cases[[1]][1],p23.eu.cases[[1]][1],p24.eu.cases[[1]][1],
  p25.eu.cases[[1]][1],p26.eu.cases[[1]][1],p27.eu.cases[[1]][1],p28.eu.cases[[1]][1],
  p1.us.cases[[1]][1]
)

trend.changes <- function(x){
  plotz <- x + 
    scale_x_datetime(date_breaks = "3 months", name = "", expand = c(0,0)) + 
    labs(y = "Trend Component (Rate Per 1,000,000 Addition") + 
    theme(axis.text.x = element_text(angle=90),
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line("gray80", size = 0.2),
          axis.line = element_line("black", size = 0.3)
          ) 
  return(plotz)
}

listz.trend.plots <- purrr::map(.x = listz.trend, .f= ~trend.changes(.x))

trend.final <- cowplot::plot_grid(plotlist = listz.trend.plots, labels = c(eu.countries, "United States"), label_x = 0.2)
ggsave(plot = trend.final, filename = "~/Desktop/trend.tiff", dpi = 300, height = 20, width = 30)
ggsave(plot = trend.final, filename = "~/Desktop/trend.pdf", dpi = 300, height = 20, width = 30)

########################################## end trend




############################################################
### Make WEEKLY  ----
library(gridExtra)
library(grid)
library(gtable)
listz.week <- c(
  p1.eu.cases[[1]][2],p2.eu.cases[[1]][2],p3.eu.cases[[1]][2],p4.eu.cases[[1]][2],
  p5.eu.cases[[1]][2],p6.eu.cases[[1]][2],p7.eu.cases[[1]][2],p8.eu.cases[[1]][2],
  p9.eu.cases[[1]][2],p10.eu.cases[[1]][2],p11.eu.cases[[1]][2],p12.eu.cases[[1]][2],
  p13.eu.cases[[1]][2],p14.eu.cases[[1]][2],p15.eu.cases[[1]][2],p16.eu.cases[[1]][2],
  p17.eu.cases[[1]][2],p18.eu.cases[[1]][2],p19.eu.cases[[1]][2],p20.eu.cases[[1]][2],
  p21.eu.cases[[1]][2],p22.eu.cases[[1]][2],p23.eu.cases[[1]][2],p24.eu.cases[[1]][2],
  p25.eu.cases[[1]][2],p26.eu.cases[[1]][2],p27.eu.cases[[1]][2],p28.eu.cases[[1]][2],
  p1.us.cases[[1]][2]
)

week.changes <- function(x){
  plotz <- x + 
    #scale_x_datetime( expand = c(0,0)) + 
    labs(y = "Weekly Component (Rate Per 1,000,000 \nAddition \n",
         x = "") + 
    theme(axis.text.x = element_text(angle=90),
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line("gray80", size = 0.2),
          axis.line = element_line("black", size = 0.3)
    ) 
  return(plotz)
}

listz.week.plots <- purrr::map(.x = listz.week, .f= ~week.changes(.x))

week.final <- cowplot::plot_grid(plotlist = listz.week.plots, labels = c(eu.countries, "United States"), label_x = 0.2)
ggsave(plot = week.final, filename = "~/Desktop/week.tiff", dpi = 300, height = 20, width = 30)
ggsave(plot = week.final, filename = "~/Desktop/week.pdf", dpi = 300, height = 20, width = 30)

########################################## end WEEKLY





############################################################
### Make stringency  ----
library(gridExtra)
library(grid)
library(gtable)
listz.reg <- c(
  p1.eu.cases[[1]][4],p2.eu.cases[[1]][4],p3.eu.cases[[1]][4],p4.eu.cases[[1]][4],
  p5.eu.cases[[1]][4],p6.eu.cases[[1]][4],p7.eu.cases[[1]][4],p8.eu.cases[[1]][4],
  p9.eu.cases[[1]][4],p10.eu.cases[[1]][4],p11.eu.cases[[1]][4],p12.eu.cases[[1]][4],
  p13.eu.cases[[1]][4],p14.eu.cases[[1]][4],p15.eu.cases[[1]][4],p16.eu.cases[[1]][4],
  p17.eu.cases[[1]][4],p18.eu.cases[[1]][4],p19.eu.cases[[1]][4],p20.eu.cases[[1]][4],
  p21.eu.cases[[1]][4],p22.eu.cases[[1]][4],p23.eu.cases[[1]][4],p24.eu.cases[[1]][4],
  p25.eu.cases[[1]][4],p26.eu.cases[[1]][4],p27.eu.cases[[1]][4],p28.eu.cases[[1]][4],
  p1.us.cases[[1]][4]
)

reg.changes <- function(x){
  plotz <- x + 
    scale_x_datetime(date_breaks = "3 months", name = "", expand = c(0,0)) + 
    labs(y = "Stringency Index Component (Rate Per 1,000,000 \nAddition \n",
         x = "") + 
    theme(axis.text.x = element_text(angle=90),
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line("gray80", size = 0.2),
          axis.line = element_line("black", size = 0.3)
    ) 
  return(plotz)
}

listz.reg.plots <- purrr::map(.x = listz.reg, .f= ~reg.changes(.x))

reg.final <- cowplot::plot_grid(plotlist = listz.reg.plots, labels = c(eu.countries, "United States"), label_x = 0.2)
ggsave(plot = reg.final, filename = "~/Desktop/reg.tiff", dpi = 300, height = 20, width = 30)
ggsave(plot = reg.final, filename = "~/Desktop/reg.pdf", dpi = 300, height = 20, width = 30)

########################################## end stringency


