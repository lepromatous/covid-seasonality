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
p1.eu.hosp <- seasonality(locs = eu.countries[1], outcome = "hosp_patients_per_million", titlez = eu.countries[1])
p2.eu.hosp <- seasonality(locs = eu.countries[2], outcome = "hosp_patients_per_million", titlez = eu.countries[2])
p3.eu.hosp <- seasonality(locs = eu.countries[3], outcome = "hosp_patients_per_million", titlez = eu.countries[3])
p4.eu.hosp <- seasonality(locs = eu.countries[4], outcome = "hosp_patients_per_million", titlez = eu.countries[4])
p5.eu.hosp <- seasonality(locs = eu.countries[5], outcome = "hosp_patients_per_million", titlez = eu.countries[5])
p6.eu.hosp <- seasonality(locs = eu.countries[6], outcome = "hosp_patients_per_million", titlez = eu.countries[6])
p7.eu.hosp <- seasonality(locs = eu.countries[7], outcome = "hosp_patients_per_million", titlez = eu.countries[7])
p8.eu.hosp <- seasonality(locs = eu.countries[8], outcome = "hosp_patients_per_million", titlez = eu.countries[8])
p9.eu.hosp <- seasonality(locs = eu.countries[9], outcome = "hosp_patients_per_million", titlez = eu.countries[9])
p10.eu.hosp <- seasonality(locs = eu.countries[10], outcome = "hosp_patients_per_million", titlez = eu.countries[10])
p11.eu.hosp <- seasonality(locs = eu.countries[11], outcome = "weekly_hosp_admissions_per_million", titlez = eu.countries[11])
p12.eu.hosp <- seasonality(locs = eu.countries[12], outcome = "weekly_hosp_admissions_per_million", titlez = eu.countries[12])
p13.eu.hosp <- seasonality(locs = eu.countries[13], outcome = "hosp_admissions_per_million", titlez = eu.countries[13])
p14.eu.hosp <- seasonality(locs = eu.countries[14], outcome = "hosp_patients_per_million", titlez = eu.countries[14])
p15.eu.hosp <- seasonality(locs = eu.countries[15], outcome = "hosp_patients_per_million", titlez = eu.countries[15])
p16.eu.hosp <- seasonality(locs = eu.countries[16], outcome = "hosp_patients_per_million", titlez = eu.countries[16])
p17.eu.hosp <- seasonality(locs = eu.countries[17], outcome = "hosp_patients_per_million", titlez = eu.countries[17])
p18.eu.hosp <- seasonality(locs = eu.countries[18], outcome = "hosp_patients_per_million", titlez = eu.countries[18])
p19.eu.hosp <- seasonality(locs = eu.countries[19], outcome = "hosp_patients_per_million", titlez = eu.countries[19])
p20.eu.hosp <- seasonality(locs = eu.countries[20], outcome = "hosp_patients_per_million", titlez = eu.countries[20])
p21.eu.hosp <- seasonality(locs = eu.countries[21], outcome = "hosp_patients_per_million", titlez = eu.countries[21])
p22.eu.hosp <- seasonality(locs = eu.countries[22], outcome = "hosp_patients_per_million", titlez = eu.countries[22])
p23.eu.hosp <- seasonality(locs = eu.countries[23], outcome = "hosp_patients_per_million", titlez = eu.countries[23])
p24.eu.hosp <- seasonality(locs = eu.countries[24], outcome = "hosp_patients_per_million", titlez = eu.countries[24])
p25.eu.hosp <- seasonality(locs = eu.countries[25], outcome = "hosp_patients_per_million", titlez = eu.countries[25])
p26.eu.hosp <- seasonality(locs = eu.countries[26], outcome = "hosp_patients_per_million", titlez = eu.countries[26])
p27.eu.hosp <- seasonality(locs = eu.countries[27], outcome = "hosp_patients_per_million", titlez = eu.countries[27])
p28.eu.hosp <- seasonality(locs = eu.countries[28], outcome = "hosp_patients_per_million", titlez = eu.countries[28])

#yo <- sapply(eu.countries, function(x) seasonality(locs = x, outcome = "hosp_patients_per_million", titlez = x))
plotz.eu.hosp <- cowplot::plot_grid(p1.eu.hosp, p2.eu.hosp, p3.eu.hosp, p4.eu.hosp, p5.eu.hosp, p6.eu.hosp, p7.eu.hosp, 
                               p8.eu.hosp, p9.eu.hosp, p10.eu.hosp, p11.eu.hosp, p12.eu.hosp, p13.eu.hosp, 
                               p14.eu.hosp, p15.eu.hosp, p16.eu.hosp, p17.eu.hosp, p18.eu.hosp, p19.eu.hosp, 
                               p20.eu.hosp, p21.eu.hosp, p22.eu.hosp, p23.eu.hosp, p24.eu.hosp, p25.eu.hosp, 
                               p26.eu.hosp, p27.eu.hosp, p28.eu.hosp)

#save_plot("~/Desktop/plot_eu.jpeg", plot = plotz.eu, base_height = 30)
#ggsave("~/Desktop/plot_eu_hosp.png", plot = plotz.eu.hosp, width = 30, height = 20, dpi = 150)




# p1.sa <- seasonality(locs = eu.countries[1], outcome = "hosp_patients_per_million", titlez = eu.countries[1])
# p2.sa <- seasonality(locs = eu.countries[2], outcome = "hosp_patients_per_million", titlez = eu.countries[2])
# p3.sa <- seasonality(locs = eu.countries[3], outcome = "hosp_patients_per_million", titlez = eu.countries[3])
# p4.sa <- seasonality(locs = eu.countries[4], outcome = "hosp_patients_per_million", titlez = eu.countries[4])
# p5.sa <- seasonality(locs = eu.countries[5], outcome = "hosp_patients_per_million", titlez = eu.countries[5])
# p6.sa <- seasonality(locs = eu.countries[6], outcome = "hosp_patients_per_million", titlez = eu.countries[6])
# p7.sa <- seasonality(locs = eu.countries[7], outcome = "hosp_patients_per_million", titlez = eu.countries[7])
# p8.sa <- seasonality(locs = eu.countries[8], outcome = "hosp_patients_per_million", titlez = eu.countries[8])
# 
# #yo <- sapply(eu.countries, function(x) seasonality(locs = x, outcome = "hosp_patients_per_million", titlez = x))
# 
# plotz <- cowplot::plot_grid(p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21,
#                             p22, p23, p24, p25, p26, p27, p28)
# 
# save_plot("~/Desktop/plot_sa.pdf", plot = plotz, base_height = 30)

## usa canada
p1.us.hosp <- seasonality(locs = "United States", outcome = "hosp_patients_per_million", titlez = "USA")
# p1.ca.hosp <- seasonality(locs = "Canada", outcome = "hosp_patients_per_million", titlez = "Canada")
# plotz.us_ca.hosp <- cowplot::plot_grid(p1.us.hosp, p1.ca.hosp, nrow = 2)
# 
# #save_plot("~/Desktop/plot_us_ca.jpeg", plot = plotz.us_ca, base_height = 10)
# ggsave("~/Desktop/plot_us_ca_hosp.png", plot = plotz.us_ca.hosp, height = 10, width=20, dpi=150)
rm(plotz.us_ca.hosp)
