
library(pacman)
pacman::p_load(tidyverse, janitor, tidyr, DT, wppExplorer, rvest)

eu.countries <-  c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom")

# pull data ----
df <- vroom::vroom("https://covid.ourworldindata.org/data/owid-covid-data.csv")

df <- subset(df, df$date>"2020-02-28" & df$date <= "2022-12-31")

df2 <- subset(df, df$location %in% c(eu.countries, "United States"))



"https://www.iban.com/country-codes" %>%
  rvest::read_html() %>%
  rvest::html_table()  %>%
  .[[1]] %>%
  janitor::clean_names() %>%
  mutate(
    numeric = stringr::str_pad(numeric, side = "left", pad = "0", width = 3)
    ) -> iso.country.codes
iso.country.codes$country[iso.country.codes$country=="United Kingdom of Great Britain and Northern Ireland (the)"] <- "United Kingdom"
iso.country.codes$country[iso.country.codes$country=="United States of America (the)"] <- "United States"
iso.country.codes$country[iso.country.codes$country=="Netherlands (the)"] <- "Netherlands"


df2 %>%
  left_join(
    iso.country.codes, by = c("location" = "country")
  ) -> df3

tpp <- wpp.indicator("tpop")
pops <- wpp.by.countries(wpp.by.year(tpp, 2020), unique(df3$alpha_2_code))
pops %>%
  janitor::clean_names() %>%
  mutate(
    pop = value *1000
  ) %>%
  dplyr::select(-value) -> pops


df3$new_hosp <- ifelse(!is.na(df3$hosp_patients), df3$hosp_patients, df3$weekly_hosp_admissions)

df3 %>%
  group_by(zoo::as.yearmon(date)) %>%
  summarise(
    total_new_cases = sum(new_cases, na.rm=T),
    total_new_hosp = sum(new_hosp, na.rm=T),
    total_new_deaths = sum(new_deaths, na.rm=T)
  ) %>%
  rename(
    moyr = 1
    )-> df4

df4$case_rate <- round((df4$total_new_cases / sum(pops$pop, na.rm=T) * 1000000),2)
df4$hosp_rate <- round((df4$total_new_hosp/ sum(pops$pop, na.rm=T) * 1000000),2)
df4$death_rate <- round((df4$total_new_deaths / sum(pops$pop, na.rm=T) * 1000000),2)


# ===================================================================
## FLU DATA
df <- vroom::vroom("https://quartzpfizer.s3.amazonaws.com/who-influenza.csv")
df %>%
  janitor::clean_names() %>%
  filter(!is.na(end_date)) -> df

df$flupos <- df$total_number_of_influenza_positive_viruses
df$fluneg <- ifelse(is.na(df$total_number_of_influenza_negative_viruses), df$processed, df$total_number_of_influenza_negative_viruses)

df$country_area_or_territory <- stringr::str_replace(df$country_area_or_territory, " \\s*\\([^\\)]+\\)", "")
df$country_area_or_territory <- gsub(")", "", df$country_area_or_territory)

eu.countries <-  c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom of Great Britain and Northern Ireland", "United States of America")

df %>%
  filter(
    country_area_or_territory %in% eu.countries
  ) %>%
  mutate(
    moyr = zoo::as.yearmon(end_date)
  )%>%
  filter(end_date >="2009-10-01"
  ) %>%
  ungroup() -> df


df %>%
  group_by(moyr) %>%
  summarise(
    flupos = sum(flupos, na.rm=T),
    fluneg = sum(fluneg, na.rm=T),
    flu_rate = flupos / (fluneg + flupos) *100
  ) -> df_flu


df_flu <- df_flu[,c("moyr", "flu_rate")]




# ========================================================================
## PLOTS

ggplot() +
  geom_line(data=df4, aes(x = moyr, y = case_rate), color = RColorBrewer::brewer.pal(n=4, "Set1")[1],
            linewidth = 1.5) +
  scale_y_continuous(n.breaks = 10, limits = c(0,70000), label = scales::comma) +
  zoo::scale_x_yearmon(n=42, expand = expansion(add = c(0.000008,0.05))) +
  labs(x = "\nMonth/Year",
       y = "COVID-19 Case Rate Per Million Population \n") +
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linewidth=0.1, "gray40"),
    panel.grid.minor.y = element_line(linewidth=0.1, "gray40"),
    axis.line = element_line(linewidth = 0.5, "black"),
    axis.text.x = element_text(angle=90, size =9)
  ) -> cases

ggplot() +
  geom_line(data=df4, aes(x = moyr, y = hosp_rate), color = RColorBrewer::brewer.pal(n=4, "Set1")[2],
            linewidth = 1.5) +
  scale_y_continuous(n.breaks = 10, limits = c(0,10500), label = scales::comma) +
  zoo::scale_x_yearmon(n=42, expand = expansion(add = c(0.000008,0.05))) +
  labs(x = "\nMonth/Year",
       y = "COVID-19 Hospitalization Rate Per Million Population \n") +
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linewidth=0.1, "gray40"),
    panel.grid.minor.y = element_line(linewidth=0.1, "gray40"),
    axis.line = element_line(linewidth = 0.5, "black"),
    axis.text.x = element_text(angle=90, size =9)
  ) -> hosp


ggplot() +
  geom_line(data=df4, aes(x = moyr, y = death_rate), color = RColorBrewer::brewer.pal(n=4, "Set1")[3],
            linewidth = 1.5) +
  scale_y_continuous(n.breaks = 12, limits = c(0,300), label = scales::comma) +
  zoo::scale_x_yearmon(n=42, expand = expansion(add = c(0.000008,0.05))) +
  labs(x = "\nMonth/Year",
       y = "COVID-19 Mortality Rate Per Million Population \n") +
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linewidth=0.1, "gray40"),
    panel.grid.minor.y = element_line(linewidth=0.1, "gray40"),
    axis.line = element_line(linewidth = 0.5, "black"),
    axis.text.x = element_text(angle=90, size =9)
  ) -> death


ggplot() +
  geom_line(data=df_flu, aes(x = moyr, y = flu_rate), color = RColorBrewer::brewer.pal(n=4, "Set1")[4],
            linewidth = 1.5) +
  scale_y_continuous(n.breaks = 10, limits = c(0,35), label = scales::comma) +
  zoo::scale_x_yearmon(n=25, expand = expansion(add = c(0.000008,0.05))) +
  labs(x = "\nMonth/Year",
       y = "Percent Influenza Virus Positivity \n") +
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linewidth=0.1, "gray40"),
    panel.grid.minor.y = element_line(linewidth=0.1, "gray40"),
    axis.line = element_line(linewidth = 0.5, "black"),
    axis.text.x = element_text(angle=90, size =9)
  ) -> flu


ggsave(plot = cases, filename = "~/Desktop/case_line.tiff", width = 10, height = 6)
ggsave(plot = hosp, filename = "~/Desktop/hosp_line.tiff", width = 10, height = 6)
ggsave(plot = death, filename = "~/Desktop/death_line.tiff", width = 10, height = 6)
ggsave(plot = flu, filename = "~/Desktop/flu_line.tiff", width = 10, height = 6)
