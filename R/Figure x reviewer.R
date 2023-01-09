
eu.countries <-  c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom")

# pull data ----
df <- vroom::vroom("https://covid.ourworldindata.org/data/owid-covid-data.csv")

df <- subset(df, df$date>"2020-02-28" & df$date <= "2022-07-31")
