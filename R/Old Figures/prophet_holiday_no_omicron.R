# build prophet model ----
# Load packages ----
library(tidyverse)
library(janitor)
library(prophet)
library(vroom)

# get data from cdc ----
setwd("~/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/estimate_booster_impact/app")
source("~/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/estimate_booster_impact/app/load_data.R")
df <- df_shiny[,c(1,13,14)]
df %>%
  filter(
    week <"2021-12-14"
  ) %>%
  group_by(week) %>%
  mutate(
    casez = sum(cases, na.rm=T),
    rate = casez / 337341954 *100000 # 
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

# add covariants as regressor ----
covariants <- vroom::vroom("https://s3.amazonaws.com/quartzdata/datasets/covariants.csv")
covariants %>%
  filter(
    state=="USA"
  ) %>%
  janitor::clean_names() %>%
  rowwise() %>%
  mutate(
    total = sum(c_across(where(is.numeric)), na.rm=T)
  ) %>%
  relocate(
    week, .before = x20b_s_732a
  ) %>%
  select(
    -state
  ) %>%
  pivot_longer(cols = contains("_"), names_to = "variant", values_to = "vals") %>%
  mutate(
    vals= vals /total *100
  ) %>%
  pivot_wider(
    names_from = "variant", values_from = "vals"
  ) %>%
  select(
    -total
  ) %>%
  mutate_all(
    ~ifelse(is.nan(.), 0, .)
  ) %>%
  replace(
    is.na(.), 0
  )-> covariants

week <- covariants$week

covariants %>%
  select(-week) %>%
  rowwise() %>%
  mutate(variant.name = names(cur_data())[which.max(c_across(everything()))]) -> covariants
covariants$week <- as.Date(week, origin="1970-01-01")

covariants %>%
  pivot_longer(
    cols = 1:29, values_to = "freq", names_to="variant"
  ) -> plot

plot %>%
  group_by(week) %>%
  filter(freq == max(freq)) -> plot

# build regressor ----
wildtype <- data_frame(
  variant = 'wildtype',
  ds = seq.Date(from=as.Date(min(df.prophet$ds)), to = as.Date("2021-04-04"), by="day")
)
delta <- data_frame(
  variant = 'delta',
  ds = seq.Date(from=as.Date("2021-04-05"), to = as.Date("2021-12-13"), by="day")
)

variants <- bind_rows(wildtype, delta)

# merge variant regressor and data ----
df.prophet2 <- merge(df.prophet, variants, by="ds", all.x=T)
df.prophet2$variant <- ifelse(df.prophet2$variant=="wildtype", 1,
                              ifelse(df.prophet2$variant == "delta", 2, NA))


# get regressor for uptake ----
uptake <- vroom::vroom("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID forecast/vaxuptake.csv")

uptake %>%
  group_by(date) %>%
  summarise(
    weighted_vax = weighted.mean(completevax, pop)
  ) %>%
  group_by(date) -> uptake

# merge and remove NA to zero ----
df.prophet2 <- merge(df.prophet2, uptake, by.x = "ds", by.y="date", all.x=T)
df.prophet2$weighted_vax[is.na(df.prophet2$weighted_vax)] <- 0

# Set up prophet ----
m <- prophet(daily.seasonality= F, 
             weekly.seasonality="auto",
             yearly.seasonality = T,
             interval.width = .95,
             seasonality.mode = 'additive',
             uncertainty.samples = 1000, 
             mcmc.samples=500) ##
# add us holidays ----
m <- add_country_holidays(m, country_name = 'US')
m <- add_regressor(m, "variant")
m <- add_regressor(m, "weighted_vax")

# forecast and decompose
m <- fit.prophet(m, df.prophet2) 
future <- make_future_dataframe(m, periods = 1)
future$variant = 3
future$weighted_vax = 0.7036409 # max of df.prophet2

forecast <- predict(m)

# get plot prophet ----
p<- prophet_plot_components(m, forecast, render_plot=F)

# extract data from prophet plot ----
test <- ggplot_build(p[[2]])$plot$data
# turn datetime to date ----
test$ds <- as.Date(test$ds)
# make year for grouping from ds ----
test$year <- lubridate::year(test$ds)


# build label range for text and boxes of year above dates in plot ----
# ymin and ymax are manually obtained for visual representation of boxes and dates
test %>%
  group_by(year) %>%
  summarise(
    xmin = min(ds),
    xmax = max(ds),
    ymin = -450,
    ymax = -350 )-> label_range

# replot ----
ggplot(data = test) + 
  geom_line(
    aes(x= ds, y=holidays)
  ) +
  geom_rect(data = label_range, fill = "#A66923", color = "#f2f2f2", alpha = 0.5,
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax,
                group = year)) +
  geom_text(data = label_range,
            aes(x = xmin + 30, y = ymin+50,
                group = year, label = year)) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels = "%b", 
    expand=c(0.0, 0.0)) +
  scale_y_continuous(
    limits = c(-450, 800), n.breaks=10
  ) + 
  geom_ribbon(
    aes(x=ds, ymin =  holidays_lower, ymax=holidays_upper),
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
  ) -> prophet_holiday


ggsave("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID forecast/manuscript/JAMA/Figures/prophet_no_omicron_holiday.pdf", width=10, height=6)


