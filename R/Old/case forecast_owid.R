# Load packages ----
library(tidyverse)
library(janitor)
library(prophet)
library(vroom)

# https://betterprogramming.pub/a-visual-guide-to-time-series-decomposition-analysis-a1472bb9c930
# https://towardsdatascience.com/time-series-analysis-with-facebook-prophet-how-it-works-and-how-to-use-it-f15ecf2c0e3a
# https://demand-planning.com/2021/02/24/practical-methods-for-identifying-seasonality-in-a-dataset/
# Load Case data ----
# df <- vroom::vroom("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
# 
# # Clean ----
# df %>%
#   pivot_longer(data = ., cols = names(df)[12:ncol(df)], names_to = "date", values_to = "cases") %>%
#   mutate(
#     date = as.Date(date, format = "%m/%d/%y")
#     ) %>%
#   select(12,13) %>%
#   group_by(date) %>%
#   summarise(
#     cases = sum(cases, na.rm=T)
#     )-> df
# 
# # compute pops by year and use for rate ----
# df$pop[lubridate::year(df$date)==2020] <- 332639102
# df$pop[lubridate::year(df$date)==2021] <- 334998398
# df$pop[lubridate::year(df$date)==2022] <- 337341954
# df$rate <- df$cases / df$pop *100000
df <- vroom::vroom("https://covid.ourworldindata.org/data/owid-covid-data.csv")
df %>%
  filter(
    location == "United States"
  ) -> df

df <- df[,c(4,13)]


# build prophet data ----
df.prophet <- data.frame(y=df[,"new_cases_smoothed_per_million"], ds=df$date, date=df$date)
df.prophet <- column_to_rownames(df.prophet, var = "date")
names(df.prophet)<-c("y", "ds")
df.prophet <- df.prophet[complete.cases(df.prophet),]
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



# ggplot(plot[plot$freq!=0,], aes(x = week, y = freq)) +
#   geom_col(fill = "#0070BF") +
#   geom_text(aes(label = variant.name), y=1, hjust="left", colour = "white", angle=90) +
#   scale_x_date(date_breaks="week") +
#   theme(
#     axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   ) +
#   labs(
#     y="Percent of Total Variants Detected \n",
#     x = "\nDate"
#   )
# ggsave("variants.png", width=10, height=6)

# build regressor ----
wildtype <- data_frame(
  variant = 'wildtype',
  ds = seq.Date(from=as.Date(min(df.prophet$ds)), to = as.Date("2021-04-04"), by="day")
)
delta <- data_frame(
  variant = 'delta',
  ds = seq.Date(from=as.Date("2021-04-05"), to = as.Date("2021-12-13"), by="day")
)
omicron <- data_frame(
  variant = 'omicron',
  ds = seq.Date(from=as.Date("2021-12-14"), to = as.Date(max(df.prophet$ds)), by="day")
)
variants <- bind_rows(wildtype, delta, omicron)

# merge variant regressor and data ----
df.prophet2 <- merge(df.prophet, variants, by="ds", all.x=T)
df.prophet2$variant <- ifelse(df.prophet2$variant=="wildtype", 1,
                              ifelse(df.prophet2$variant == "delta", 2, 3))

# # clean ----
# rm(list=setdiff(ls(), "df"))
# gc()

# Set up prophet ----
m <- prophet(daily.seasonality= F, 
             weekly.seasonality="auto",
             yearly.seasonality = "auto",
             interval.width = .8,
             seasonality.mode = 'additive') ## multiplicative when seasonality grows with the trend
# add us holidays ----
m <- add_country_holidays(m, country_name = 'US')
#m <- add_regressor(m, "variant")

#m <- add_seasonality(m, name='quarterly', period=91.3125, fourier.order=5)
#m <- add_seasonality(m, name='summer', period=365.25/2, fourier.order=5)

# df.prophet$cap <- 30000 ## max needed for logistic growth
# forecast and decompose
m <- fit.prophet(m, df.prophet2) 
future <- make_future_dataframe(m, periods = 1)
future$variant = 3
#future$cap <- 30000 ## max needed for logistic growth
forecast <- predict(m, future)

# edit prophet ----
library(prophet)
p<- prophet_plot_components(m, forecast, render_plot=F)

p[[1]] + 
  scale_x_datetime(date_breaks = "6 weeks") +
  labs(
    x="",
    y="Trend Effect"
  ) +
  theme(
    axis.text.x = element_text(angle=90),
    panel.background = element_blank(),
    axis.line = element_line("black", size=0.2),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y =element_blank(),
    panel.grid.major.x = element_line("black", 0.05)
  ) -> p_trend

p[[2]] + 
  scale_x_datetime(date_breaks = "6 weeks")  +
  labs(
    x="",
    y="Holiday Effect"
  ) +
  theme(
    axis.text.x = element_text(angle=90),
    panel.background = element_blank(),
    axis.line = element_line("black", size=0.2),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y =element_blank(),
    panel.grid.major.x = element_line("black", 0.05)  ) -> p_holiday

p[[3]] + 
  scale_x_datetime(date_breaks = "3 weeks")  +
  labs(
    x="",
    "Yearly Effect"
  ) +
  theme(
    axis.text.x = element_text(angle=90),
    panel.background = element_blank(),
    axis.line = element_line("black", size=0.2),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y =element_blank(),
    panel.grid.major.x = element_line("black", 0.05)  ) -> p_year

library(cowplot)
plot_grid(
  p_trend, p_holiday, p_year,
  labels = "", ncol = 1
) -> p2
p2
#plot(m, forecast)

save_plot("~/Desktop/prophet.png", p2, base_width=8, base_height=7)


max(df.prophet2$ds)
# try tbats per: https://stats.stackexchange.com/questions/57705/identify-seasonality-in-time-series-data
x <- ts(df.prophet2[,2], start=c(2020,1), end=c(2022,4), frequency = 365)
fit <- forecast::tbats(x)
seasonal <- !is.null(fit$seasonal)
seasonal
# doesnt work well - probably b/c times are messy. 


### try new plot ----https://robjhyndman.com/papers/MPcomments.pdf
library(forecast)
ggAcf(x)
ggPacf(x)
ggtaperedacf(x)


### try anomaly - YES
library(anomalize)
df.prophet %>% 
  tibble() %>%
  time_decompose(y, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
  plot_anomaly_decomposition() -> p

p +
  labs(
    x="\n Date",
    y = "Value \n"
  ) +
  scale_x_date(date_breaks="4 weeks") +
  theme(
    axis.text.x = element_text(angle=90)
  )
ggsave("~/Desktop/anomaly.png")

# # seasonality plot
# df.prophet %>%
#   tibble() %>%
#   mutate(
#     year = lubridate::year(ds)
#     ) %>%
#   #group_by(year) %>%
#   mutate(
#     y = scale(y)
#     )-> df.seasonal

# seasonality plot
df.prophet %>%
  tibble() %>%
  mutate(
    year = lubridate::year(ds),
    y = scale(y)
  ) -> df.seasonal

table(df.seasonal$year)
ggplot()+
  geom_smooth(data=df.seasonal[df.seasonal$year==2020,], aes(x=seq(27,365), y=y)) +
  geom_smooth(data=df.seasonal[df.seasonal$year==2021,], aes(x=seq(1,365), y=y)) +
  geom_smooth(data=df.seasonal[df.seasonal$year==2022,], aes(x=seq(1,104), y=y)) +
  scale_x_continuous(labels = month.abb, breaks = seq(1,365, by=30)[-13]) +
  # annotate("text", x=9, y=0, label="2020") +
  # annotate("text", x=1, y=500, label="2021") +
  # annotate("text", x=6, y=1800, label="2022") +
   theme(
    panel.background = element_blank(),
    axis.line = element_line("lightgray", 0.2)
  ) +
  labs(
    y= "Smoothed Rate Per 1,000,000 Population\n",
    x="\nMonth of Year"
  )


#   
# summary(lm(y ~ seq(10,52), data = df.seasonal[df.seasonal$year==2020,]))
# summary(lm(y ~ seq(1,52), data = df.seasonal[df.seasonal$year==2021,]))
# summary(lm(y ~ seq(1,15), data = df.seasonal[df.seasonal$year==2022,]))

