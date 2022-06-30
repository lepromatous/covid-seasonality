# Load packages ----
library(tidyverse)
library(janitor)
library(prophet)
library(vroom)
library(anomalize)


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

df.prophet_yearmon <- df.prophet
df.prophet_yearmon %>%
  mutate(
    moyr = zoo::as.yearmon(ds)
  ) -> df.prophet_yearmon

df.prophet_yearmon %>%
  #filter(ds < "2021-12-14") %>%
  tibble() %>%
  time_decompose(y, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
  plot_anomaly_decomposition() -> p

p +
  labs(
    x="\n Date",
    y = "Value \n"
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand=c(0.01,0)) +
  scale_y_continuous(expand=c(0.2,0.1)) +
  theme(
    axis.text.x = element_text(angle=90)
  ) -> p2

p2[[1]] -> yo

yo$ds <- as.Date(yo$ds)
yo$year <- lubridate::year(yo$ds)

yo %>%
  group_by(year) %>%
  summarise(
    xmin = min(ds),
    xmax = max(ds),
    ymin = -30,
    ymax = 0 )-> label_range

leg <-  tibble(
  colour = ifelse(yo[yo$key=="season","anomaly"]=="Yes", "red", "black"),
  size = ifelse(yo[yo$key=="season","anomaly"]=="Yes", 1.5,1),
  size2 = ifelse(yo[yo$key=="season","anomaly"]=="Yes", 3,1),
  shape = ifelse(yo[yo$key=="season","anomaly"]=="Yes", 1,19),
  size_disappear = ifelse(yo[yo$key=="season","anomaly"]=="Yes", 3,1),
  stroke = ifelse(yo[yo$key=="season","anomaly"]=="Yes", 1.5,0.3)
)

# replot ----
ggplot(data = yo[yo$key=="season",]) + 
  # geom_line(
  #   aes(x= ds, y=value)
  # ) +
  geom_point(
    aes(x=ds, y=value, colour = leg$colour, size = leg$size_disappear, shape = leg$shape), 
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
                ymin = ymin, ymax = ymax-25,
                group = year)) +
  geom_text(data = label_range,
            aes(x = xmin+30, y = ymin+2,
                group = year, label = year)) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels = "%b", 
    expand=c(0.0, 0.0)) +
  scale_y_continuous(
    limits = c(-30, 25), n.breaks=12
  ) + 
  labs(
    colour = "Anomaly",
    x="",
    y=""
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
                                                   stroke = c(0.8, 2)))) -> season
season
ggsave("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID forecast/manuscript/JAMA/Figures/anomaly_all_season.pdf", width=10, height=6)

