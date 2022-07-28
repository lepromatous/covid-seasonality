type = "cases"

# extract lower bound of yearly from annual seasonal component
d <- grep("p",names(.GlobalEnv),value=TRUE)

type <- sub(".*\\.", "", d)[1]
type <- substr(type, start=1, stop = nchar(type)-1)

# remove items that dont have plots ----
d <- d[!d %in% c("type", "country.lookup", "listz.reg.plots", "listz.week.plots", "listz.trend.plots", "plotz.eu.hosp",
                 "p5.eu.flu", "p6.eu.flu", "p26.eu.flu", "p28.eu.flu")] 
# pull out clean prophet plots from list of names and save plot
season <- do.call(rbind, lapply(d, get))
#season <- season[((length(season)/2)+1):length(season)] # do.call extracts first element, then rbinds 2nd element - doesnt interleave ----
# do.call extracts first element, then rbinds 2nd element - doesnt interleave ----
# arrange season in order we want to see ----
d %>%
  tibble() %>%
  dplyr::arrange(
    readr::parse_number(.)
  ) -> out

orderit <- match(out[[1]], d)
season <- season[orderit]
season <- season[c(1, 3:27, 2)]  # verify country order with d, out, orderit. 

new.eu.countries <- c(eu.countries[-c(5,6,26:28)], "United Kingdom", "United States")

out <- data.table::rbindlist(season)
out$country <- rep(new.eu.countries, each = 365)
out %>%
  group_by(country, lubridate::month(ds)) %>%
  summarise(
    rate = median(yearly)
  ) %>%
  rename(
    rate = 3,
    country = 1, 
    month = 2
  )-> out
  
minz <- min(out$rate)
maxz <- max(out$rate)

ggplot() +
  geom_tile(data=out, aes(x = month, y = country, fill = rate)) +
  coord_fixed() + 
  scale_fill_gradient2(low = "#67BB6E", mid = "white", high = "#0095FF", breaks = c(minz, 0,  maxz), labels = c(round(minz,0), 0, round(maxz,0))) + 
  scale_x_continuous(breaks = c(seq(1:12)), labels = c(month.abb)) + 
  labs(
    x = "",
    y = "",
    fill = "Annual Seasonal \nAddition to \nRate Per 100,000"
  ) +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle=90, vjust=0.5)
  ) 


    ggsave(paste0("~/Desktop/heatmap_flu_", type, ".pdf"), height = 9, dpi = 300)
    ggsave(paste0("~/Desktop/heatmap_flu_", type, ".tiff"), height = 9, dpi = 300)

    #rm(list = ls())


df %>%
  group_by(country_area_or_territory) %>%
  filter(country_area_or_territory %in% eu.countries) %>%
  summarise(
    table(!is.na(y))
  ) -> miss
