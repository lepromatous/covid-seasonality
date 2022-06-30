# R
library(dplyr)
christmas2020 <- data_frame(
  holiday = 'Christmas2020',
  ds = seq.Date(as.Date('2020-12-21'), as.Date( '2021-01-03'), by='day'),
  lower_window = 0,
  upper_window = 0
)
christmas2020 <- subset(christmas2020, christmas2020$ds!="2020-12-25")

christmas2021 <- data_frame(
  holiday = 'Christmas2021',
  ds = seq.Date(as.Date('2021-12-20'), as.Date( '2022-01-02'), by='day'),
  lower_window = 0,
  upper_window = 0
)
christmas2021 <- subset(christmas2021, christmas2021$ds!="2021-12-25")

springbreak2020 <- data_frame(
  holiday = 'springbreak2020',
  ds = seq.Date(as.Date('2020-03-16'), as.Date('2020-03-27'), by='day'),
  lower_window = 0,
  upper_window = 0
)

springbreak2021 <- data_frame(
  holiday = 'springbreak2021',
  ds = seq.Date(as.Date('2021-03-15'), as.Date('2021-03-26'), by='day'),
  lower_window = 0,
  upper_window = 0
)
springbreak2022 <- data_frame(
  holiday = 'springbreak2022',
  ds = seq.Date(as.Date('2022-03-14'), as.Date('2022-03-25'), by='day'),
  lower_window = 0,
  upper_window = 0
)




holidays_extra <- bind_rows(christmas2020, christmas2021,  springbreak2020, springbreak2021, springbreak2022)