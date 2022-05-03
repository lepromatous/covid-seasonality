### load data hospitalization data.  stops at march 12

df <- df_shiny_hosp[,c(1,2,13,15)]


df %>%
  group_by(week) %>%
  summarise(
    cases = sum(corrected_cases)
  ) -> df


library(prophet)


m <- prophet(daily.seasonality= F, 
             weekly.seasonality=F,
             yearly.seasonality = T,
             interval.width = .8)
## add us holidays
m <- add_country_holidays(m, country_name = 'US')
# ---------------------------------------------------------------------------------------
df.prophet <- data.frame(y=df[,"cases"], ds=df$week, date=df$week)
df.prophet <- column_to_rownames(df.prophet, var = "date")
names(df.prophet)<-c("y", "ds")

m <- fit.prophet(m, df.prophet) 
future <- make_future_dataframe(m, periods = 60)
forecast <- predict(m, future)



p <- plot(m, forecast) 

p +
  scale_y_continuous(limits=c(0,120000), labels=scales::comma) + 
  scale_x_datetime(date_breaks = "4 weeks", expand=c(0.02, 0.02)) + 
  labs(
    y = "Total Number of Cases\n",
    x = "\nWeek Ending"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color="gray80", size=0.2),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) -> yo

library(plotly)
yo <- ggplotly(yo)


htmlwidgets::saveWidget(yo, "~/Desktop/forecast_60d.html")



