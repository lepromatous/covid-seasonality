type = "hospitalizations"

# extract lower bound of yearly from annual seasonal component
d <- grep("p",names(.GlobalEnv),value=TRUE)

type <- sub(".*\\.", "", d)[1]
type <- substr(type, start=1, stop = nchar(type)-1)

# remove items that dont have plots ----
d <- d[!d %in% c("type", "country.lookup", "listz.reg.plots", "listz.week.plots", "listz.trend.plots", "plotz.eu.hosp")] 
# pull out clean prophet plots from list of names and save plot
season <- do.call(rbind, lapply(d, get))
# 2nd set stacked is what we need for these plots - extract
season <- season[((length(season)/2)+1):length(season)] # do.call extracts first element, then rbinds 2nd element - doesnt interleave ----

# arrange season in order we want to see ----
d %>%
  tibble() %>%
  dplyr::arrange(
    readr::parse_number(.)
  ) -> out # shows current order
orderit <- match(out[[1]], d) # match d to order we want

# re arrange season to order we want
season <- season[orderit]
# put US last. 
season <- season[c(2:29, 1)]

# save
library(gridExtra)
ggsave(plot = grid.arrange(grobs = season), filename = paste0("~/Desktop/season", type, ".tiff"), height = 20, width = 30, dpi = 300)

ggsave(plot = grid.arrange(grobs = season), filename = paste0("~/Desktop/season", type, ".pdf"), height = 20, width = 30, dpi = 300)








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

# re arrange season to order we want
season2 <- season2[orderit]
# put US last. 
season2 <- season2[c(2:29, 1)]

#extract data from each plot for replotting heatmap
season2_lower <- lapply(season2, function(x) x[[3]]$data$yearly_lower)
season2_upper <- lapply(season2, function(x) x[[3]]$data$yearly_upper)

season2 <- lapply(season2, function(x) x[[3]]$data$yearly)

#unlist to make usable data
season3 <- data.frame("rate" = unlist(season2))

# add country label per above reorder
season3$country <- rep(c(eu.countries, "United States"), easy= 365)

# add dates as these are daily data starting march 1, 2020    
season3$date <- rep(seq.Date(as.Date("2020-01-01"), by="day", length.out=365), times=length(season))

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
    fill = "Annual Seasonal \nAddition to \nRate Per 100,000"
  ) +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle=90, vjust=0.5)
  ) 

ggsave(paste0("~/Desktop/heatmap_", type, ".pdf"), height = 9, dpi = 300)
ggsave(paste0("~/Desktop/heatmap_", type, ".tiff"), height = 9, dpi = 300)

