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

# end date ----


# Load packages ----
library(tidyverse)
library(janitor)
library(prophet)
library(vroom)
library(anomalize)

# make plot ----
df.prophet <- data.frame(y=df.prophet[,"y"], ds=df.prophet$ds, date=df.prophet$ds)
df.prophet <- column_to_rownames(df.prophet, var = "ds")
names(df.prophet)<-c("y", "ds")


df.prophet %>%
  #filter(ds < "2021-12-14") %>%
  tibble() %>%
  time_decompose(y, method = "twitter", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.3) %>%
  time_recompose() -> yo

# p +
#   labs(
#     x="\n Date",
#     y = "Value \n"
#   ) +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand=c(0.01,0)) +
#   scale_y_continuous(expand=c(0.2,0.1)) +
#   theme(
#     axis.text.x = element_text(angle=90)
#   ) -> p2



yo$ds <- as.Date(yo$ds)
yo$year <- lubridate::year(yo$ds)

yo %>%
  group_by(year) %>%
  summarise(
    xmin = min(ds),
    xmax = max(ds),
    ymin = -2000,
    ymax = 0 )-> label_range

leg <-  tibble(
  colour = ifelse(yo[,"anomaly"]=="Yes", "red", "black"),
  size = ifelse(yo[,"anomaly"]=="Yes", 1.5,1),
  size2 = ifelse(yo[,"anomaly"]=="Yes", 3,1),
  shape = ifelse(yo[,"anomaly"]=="Yes", 1,19),
  size_disappear = ifelse(yo[,"anomaly"]=="Yes", 3,1),
  stroke = ifelse(yo[,"anomaly"]=="Yes", 1.5,0.8)
)

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
                ymin = ymin-2000, ymax = ymax-2500,
                group = year)) +
  geom_text(data = label_range,
            aes(x = xmin+50, y = ymin-1250,
                group = year, label = year), size=3) +
  geom_ribbon(
    aes(x= ds, ymin = recomposed_l1, ymax=recomposed_l2), color = "#272936", alpha=0.2
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels = "%b", 
    expand=c(0.02, 0.05)) +
  scale_y_continuous(
    limits = c(-4000, 30000), n.breaks=12
  ) + 
  labs(
    colour = "Anomaly",
    x="",
    y="Total Frequency of Influenza Specimens"
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
                                                   stroke = c(0.8, 2)))) -> observed
observed
ggsave("~/Desktop/influenza.pdf", width=14, height=6)

