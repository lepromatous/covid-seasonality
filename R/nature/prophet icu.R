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



#season <- function(){
  
  eu.countries <-  c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom")
  sa.countries <- c("Columbia", "Brazil", "Peru", "Ecuador", "Bolivia", "Guyana", "Suriname", "Venezuela")
  
  # pull data ----
  df <- vroom::vroom("https://covid.ourworldindata.org/data/owid-covid-data.csv")

  # Set up prophet ----
  m <- prophet(daily.seasonality= F, 
               weekly.seasonality=T,
               yearly.seasonality = T,
               interval.width = .95,
               seasonality.mode = 'additive',
               uncertainty.samples = 1000, 
               mcmc.samples=500) ##

  
seasonality <- function(locs, outcome, titlez){
  df <- subset(df, df$location == locs)
  
  df <- df[,c("date", outcome)]
  names(df) <- c("ds", "y")
  # forecast and decompose
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
p1.eu.icu <- seasonality(locs = eu.countries[1], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[1])
p2.eu.icu <- seasonality(locs = eu.countries[2], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[2])
p3.eu.icu <- seasonality(locs = eu.countries[3], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[3])
p4.eu.icu <- seasonality(locs = eu.countries[4], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[4])
p5.eu.icu <- seasonality(locs = eu.countries[5], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[5])
p6.eu.icu <- seasonality(locs = eu.countries[6], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[6])
p7.eu.icu <- seasonality(locs = eu.countries[7], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[7])
p8.eu.icu <- seasonality(locs = eu.countries[8], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[8])
p9.eu.icu <- seasonality(locs = eu.countries[9], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[9])
p10.eu.icu <- seasonality(locs = eu.countries[10], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[10])
p11.eu.icu <- seasonality(locs = eu.countries[11], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[11])
p12.eu.icu <- seasonality(locs = eu.countries[12], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[12])
p13.eu.icu <- seasonality(locs = eu.countries[13], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[13])
p14.eu.icu <- seasonality(locs = eu.countries[14], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[14])
p15.eu.icu <- seasonality(locs = eu.countries[15], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[15])
p16.eu.icu <- seasonality(locs = eu.countries[16], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[16])
p17.eu.icu <- seasonality(locs = eu.countries[17], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[17])
p18.eu.icu <- seasonality(locs = eu.countries[18], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[18])
p19.eu.icu <- seasonality(locs = eu.countries[19], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[19])
p20.eu.icu <- seasonality(locs = eu.countries[20], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[20])
p21.eu.icu <- seasonality(locs = eu.countries[21], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[21])
p22.eu.icu <- seasonality(locs = eu.countries[22], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[22])
p23.eu.icu <- seasonality(locs = eu.countries[23], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[23])
p24.eu.icu <- seasonality(locs = eu.countries[24], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[24])
p25.eu.icu <- seasonality(locs = eu.countries[25], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[25])
p26.eu.icu <- seasonality(locs = eu.countries[26], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[26])
p27.eu.icu <- seasonality(locs = eu.countries[27], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[27])
p28.eu.icu <- seasonality(locs = eu.countries[28], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[28])

#yo <- sapply(eu.countries, function(x) seasonality(locs = x, outcome = "weekly_icu_admissions_per_million", titlez = x))
plotz.eu.icu <- cowplot::plot_grid(p1.eu.icu, p2.eu.icu, p3.eu.icu, p4.eu.icu, p5.eu.icu, p6.eu.icu, p7.eu.icu, 
                               p8.eu.icu, p9.eu.icu, p10.eu.icu, p11.eu.icu, p12.eu.icu, p13.eu.icu, 
                               p14.eu.icu, p15.eu.icu, p16.eu.icu, p17.eu.icu, p18.eu.icu, p19.eu.icu, 
                               p20.eu.icu, p21.eu.icu, p22.eu.icu, p23.eu.icu, p24.eu.icu, p25.eu.icu, 
                               p26.eu.icu, p27.eu.icu, p28.eu.icu)

#save_plot("~/Desktop/plot_eu.jpeg", plot = plotz.eu, base_height = 30)
ggsave("~/Desktop/plot_eu_icu.png", plot = plotz.eu.icu, width = 30, height = 20, dpi = 150)




# p1.sa <- seasonality(locs = eu.countries[1], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[1])
# p2.sa <- seasonality(locs = eu.countries[2], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[2])
# p3.sa <- seasonality(locs = eu.countries[3], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[3])
# p4.sa <- seasonality(locs = eu.countries[4], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[4])
# p5.sa <- seasonality(locs = eu.countries[5], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[5])
# p6.sa <- seasonality(locs = eu.countries[6], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[6])
# p7.sa <- seasonality(locs = eu.countries[7], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[7])
# p8.sa <- seasonality(locs = eu.countries[8], outcome = "weekly_icu_admissions_per_million", titlez = eu.countries[8])
# 
# #yo <- sapply(eu.countries, function(x) seasonality(locs = x, outcome = "weekly_icu_admissions_per_million", titlez = x))
# 
# plotz <- cowplot::plot_grid(p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21,
#                             p22, p23, p24, p25, p26, p27, p28)
# 
# save_plot("~/Desktop/plot_sa.pdf", plot = plotz, base_height = 30)

## usa canada
p1.us.icu <- seasonality(locs = "United States", outcome = "weekly_icu_admissions_per_million", titlez = "USA")
p1.ca.icu <- seasonality(locs = "Canada", outcome = "weekly_icu_admissions_per_million", titlez = "Canada")
plotz.us_ca.icu <- cowplot::plot_grid(p1.us.icu, p1.ca.icu, nrow = 2)

#save_plot("~/Desktop/plot_us_ca.jpeg", plot = plotz.us_ca, base_height = 10)
ggsave("~/Desktop/plot_us_ca_icu.png", plot = plotz.us_ca.icu, height = 10, width=20, dpi=150)

