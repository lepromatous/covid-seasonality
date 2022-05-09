# Load packages ----
library(tidyverse)
library(janitor)
library(prophet)
library(vroom)
library(anomalize)
library(RSocrata)
library(rvest)
# get data from cdc ----
setwd("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/estimate_booster_impact/app")
source("~/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/estimate_booster_impact/app/load_data.R")
df <- df_shiny[,c(1,13,14)]
df %>%
  group_by(week) %>%
  mutate(
    casez = sum(cases, na.rm=T),
    rate = casez / 337341954 *100000
  ) %>%
  slice(1) %>%
  select(c(1,5))-> df

# clean ----
rm(list=setdiff(ls(), "df"))
gc()

# build prophet data ----
df.prophet <- data.frame(y=df[,"rate"], ds=df$week, date=df$week)
df.prophet <- column_to_rownames(df.prophet, var = "date")
names(df.prophet)<-c("y", "ds")


# build prophet data ----
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
    ymin = -200,
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
                ymin = ymin, ymax = ymax-100,
                group = year)) +
  geom_text(data = label_range,
            aes(x = xmin+25, y = ymin+50,
                group = year, label = year)) +
  geom_ribbon(
    aes(x= ds, ymin = recomposed_l1, ymax=recomposed_l2), color = "#272936", alpha=0.2
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels = "%b", 
    expand=c(0.01, 0.0)) +
  scale_y_continuous(
    limits = c(-200, 2000, by=200)
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
    legend.position = c(0.2,0.9),
    legend.direction = "horizontal" 
  ) +
  guides(colour = guide_legend(override.aes = list(size = c(1,5),
                                                   shape = c(19,1),
                                                   stroke = c(0.8, 2)))) -> observed_recomposed
observed_recomposed
ggsave("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/covid-seasonality/Manuscript/Lancet/Figures/anomaly CDC/anomaly_CDC.pdf", width=10, height=6)
