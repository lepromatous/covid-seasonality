type = "flu"

# extract lower bound of yearly from annual seasonal component
d <- grep("p",names(.GlobalEnv),value=TRUE)

type <- sub(".*\\.", "", d)[1]
type <- substr(type, start=1, stop = nchar(type)-1)

# remove items that dont have plots ----
d <- d[!d %in% c("type", "country.lookup", "listz.reg.plots", "listz.week.plots", "listz.trend.plots", "plotz.eu.hosp")] 

#########################
# now get data from list to use for heatmaps
#########################
# turn d to data frame - same general approach as above - 1st set is what we need for raw data
season2 <- do.call(rbind, lapply(d, get))
season2 <- season2[1:((length(season2)/2))] 
# arrange season in order we want to see ----
d %>%
  tibble() %>%
  dplyr::arrange(
    readr::parse_number(.)
  ) -> out # shows current order
orderit <- match(out[[1]], d) # match d to order we want

# re arrange season to order we want, US is already last
season2 <- season2[orderit]

season2 <- lapply(season2, function(x) x$yearly)

#unlist to make usable data
season3 <- data.frame("rate" = unlist(season2))

# add country label per above reorder
season3$country <- rep(c(eu.countries), easy= 365)

# add dates as these are daily data starting march 1, 2020    
season3$date <- rep(seq.Date(as.Date("2020-01-01"), by="day", length.out=365), times=length(season2))

# monthly aggregate 
season3 %>%
  group_by(country, lubridate::month(date)) %>%
  summarise(
    med.rate = median(rate, na.rm=T)
  ) %>%
  rename(
    rate = 3,
    country = 1, 
    month = 2
  ) %>%
  ungroup() -> out

# min max for all countries for plotting
minz <- min(out$rate)
maxz <- max(out$rate)

# fix labels to match fig 1
out$country[out$country == 'United Kingdom of Great Britain and Northern Ireland'] <- "United Kingdom"
out$country[out$country == 'United States of America'] <- "United States"

ggplot() +
  geom_tile(data=out, aes(x = month, y = country, fill = rate)) +
  coord_fixed() + 
  scale_fill_gradient2(low = "#67BB6E", mid = "white", high = "#0095FF", 
                       breaks = c(minz, 0,  maxz), 
                       labels = c(round(minz,0), 0, round(maxz,0))) + 
  scale_x_continuous(breaks = c(seq(1:12)), labels = c(month.abb)) + 
  labs(
    x = "",
    y = "",
    fill = "Annual Seasonal \nAddition to \nRate Per Million"
  ) +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle=90, vjust=0.5)
  ) 

ggsave(paste0("~/Desktop/heatmap_", type, ".pdf"), height = 9, dpi = 300)
ggsave(paste0("~/Desktop/heatmap_", type, ".tiff"), height = 9, dpi = 300)

