# Load packages ----
library(janitor)
library(tidyverse)
library(prophet)
library(rvest)
library(tidyverse)
library(cdcfluview)

fluviewr_data<-function(regionz = "national"){
  df <- cdcfluview::who_nrevss(region = "national")
  df[[1]] %>%
    rowwise %>%
    mutate(
      a_all = sum(a_2009_h1n1, a_h1, a_subtyping_not_performed,
                  a_h3, a_unable_to_subtype, h3n2v),
      b_all = sum(b) 
    )%>%
    select(-c(a_h1, a_h3, a_2009_h1n1, percent_positive, 
              a_subtyping_not_performed, a_unable_to_subtype,
              h3n2v, b))-> df_old
  df[[2]] %>%
    rowwise %>%
    mutate(
      a_all = sum(a_2009_h1n1, a_h3, a_subtyping_not_performed, h3n2v),
      b_all = sum(b, bvic, byam)
    )  %>%
    select(-c(a_2009_h1n1, a_h3,
              a_subtyping_not_performed,
              h3n2v, b, bvic, byam))-> df_ph
  df[[3]] %>%
    rename(
      a_all = "total_a",
      b_all = "total_b"
    ) %>%
    select(-c(percent_positive,
              percent_a,
              percent_b)) -> df_clin
  
  df_new <- merge(df_ph, df_clin, by="wk_date", all=T)
  df_new$total_specimens <- rowSums(df_new[, c("total_specimens.x", "total_specimens.y")])
  df_new$a_all <- rowSums(df_new[,c("a_all.x", "a_all.y")])
  df_new$b_all <- rowSums(df_new[,c("b_all.x", "b_all.y")])
  
  df_new %>%
    rename(
      region_type = "region_type.x",
      year = "year.x",
      week = "week.x",
      region = "region.x",
    ) -> df_new
  df_new <- df_new[,names(df_old)]
  
  df <- rbind(df_old, df_new)
  
  df$total_flu <- rowSums(df[,c("a_all", "b_all")])
  df$percent_a <- round(df$a_all / df$total_specimens *100,1)
  df$percent_b <- round(df$b_all / df$total_specimens *100,1)
  return(df)
}



# get data - run scrips first
source("https://raw.githubusercontent.com/lepromatous/fluviewR/main/fluview_functions_2.R")
df <- fluviewr_data()

df <- df[,c("wk_date", "total_flu")]

df%>%
  rename(
    ds = 1,
    y = 2
  ) -> df


df %>%
  filter(
    ds <= "2020-04-10",
    ds >= "2014-03-01"
  ) -> df.prophet

# end  ----




# make plot ----
df.prophet <- data.frame(y=df.prophet[,"y"], ds=df.prophet$ds, date=df.prophet$ds)
df.prophet <- column_to_rownames(df.prophet, var = "ds")
names(df.prophet)<-c("y", "ds")





# Set up prophet ----
m <- prophet(daily.seasonality= F, 
             weekly.seasonality="auto",
             yearly.seasonality = T,
             interval.width = .95,
             seasonality.mode = 'additive',
             uncertainty.samples = 2000, 
             mcmc.samples=1000) ##
# add us holidays ----
m <- add_country_holidays(m, country_name = 'US')

# forecast and decompose
m <- fit.prophet(m, df.prophet) 

forecast <- predict(m)

# get plot prophet ----
p<- prophet_plot_components(m, forecast, render_plot=F)



# extract data from prophet plot ----
test <- ggplot_build(p[[1]])$plot$data
# turn datetime to date ----
test$ds <- as.Date(test$ds)
# make year for grouping from ds ----
test$year <- lubridate::year(test$ds)


# extract, merge and compute residuals
yo <- merge(test, df, by.x="ds", by.y="ds")
residuals <- yo[,'yhat'] - yo[,'y']
residuals.lo <- yo[,'yhat_lower'] - yo[,'y']
residuals.hi <- yo[,'yhat_upper'] - yo[,'y']

test_resid <- tibble(
  ds = yo$ds,
  residuals,
  residuals.lo,
  residuals.hi,
  year = lubridate::year(ds)
)



test_resid %>%
  group_by(year) %>%
  summarise(
    xmin = min(ds),
    xmax = max(ds),
    ymin = -20000,
    ymax = -19000 )-> label_range

# replot ----

# replot ----
ggplot(data = test_resid) + 
  geom_line(
    aes(x= ds, y=residuals)
  ) +
  geom_rect(data = label_range, fill = "#A66923", color = "#f2f2f2", alpha = 0.5,
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax,
                group = year)) +
  geom_text(data = label_range,
            aes(x = xmin + 50, y = ymin+500,
                group = year, label = year), size=3) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels = "%b", 
    expand=c(0.0, 0.0)) +
  scale_y_continuous(
    limits = c(-20000, 20000), n.breaks=10
  ) + 
  geom_ribbon(
    aes(x=ds, ymin =  residuals.lo, ymax=residuals.hi),
    fill = "#88B0AD", alpha = 0.5
  ) + 
  labs(
    x="",
    y=""
  ) + 
  theme(
    axis.text.x = element_text(angle=90),
    panel.background = element_blank(),
    axis.line = element_line("black", size=0.2),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y =element_blank(),
    panel.grid.major.x = element_line("gray90", 0.05)
  ) -> prophet_residuals_flu

prophet_residuals_flu
ggsave("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/covid-seasonality/Manuscript/Lancet/Figures/prophet_US_residuals_flu.pdf", width=10, height=6)



