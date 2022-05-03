### try anomaly - YES
df.prophet_yearmon <- df.prophet
df.prophet_yearmon %>%
  mutate(
    moyr = zoo::as.yearmon(ds)
  ) -> df.prophet_yearmon

library(anomalize)
df.prophet_yearmon %>% 
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
  )
