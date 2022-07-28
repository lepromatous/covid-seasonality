type = "hosp"

# extract lower bound of yearly from annual seasonal component
d <- grep("p",names(.GlobalEnv),value=TRUE)

type <- sub(".*\\.", "", d)[1]
type <- substr(type, start=1, stop = nchar(type)-1)

# remove items that dont have plots ----
d <- d[!d %in% c("type", "country.lookup", "listz.reg.plots", "listz.week.plots", "listz.trend.plots", "plotz.eu.hosp")] 
# pull out clean prophet plots from list of names and save plot
season <- do.call(rbind, lapply(d, get))
season <- season[((length(season)/2)+1):length(season)] # do.call extracts first element, then rbinds 2nd element - doesnt interleave ----
# do.call extracts first element, then rbinds 2nd element - doesnt interleave ----

# arrange season in order we want to see ----
d %>%
  tibble() %>%
  dplyr::arrange(
    readr::parse_number(.)
  ) -> out
orderit <- match(out[[1]], d)


season <- season[orderit]
season <- season[c(2:29, 1)]

ggsave(plot = grid.arrange(grobs = season), filename = paste0("~/Desktop/season", type, ".tiff"), height = 20, width = 30, dpi = 300)

ggsave(plot = grid.arrange(grobs = season), filename = paste0("~/Desktop/season", type, ".pdf"), height = 20, width = 30, dpi = 300)




# save plot ---- 
### SUPPLEMENT PLOTS
# ggsave(plot = grid.arrange(grobs = season), filename = paste0("~/Desktop/season", type, ".tiff"), height = 20, width = 30, dpi = 300)
# ggsave(plot = grid.arrange(grobs = season), filename = paste0("~/Desktop/season", type, ".pdf"), height = 20, width = 30, dpi = 300)
# 








#########################
# now get data from list to use for heatmaps
#########################
# turn d to data frame
season2 <- do.call(rbind, lapply(d, get))
season2 <- season[1:((length(season2)/2))] 
season2 <- lapply(season2, function(x) x$data$yearly)
season2 <- season2[mod]
season2 <- data.frame("rate" = unlist(season2))
season2$country <- rep(c(eu.countries, "United States"), easy= 365)
    
season2$date <- rep(seq.Date(as.Date("2020-01-01"), by="day", length.out=365), times=length(season))
    
  season2 %>%
      group_by(country, lubridate::month(date)) %>%
      summarise(
        med.rate = median(rate, na.rm=T)
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
    
    ggsave(paste0("~/Desktop/heatmap_", type, ".pdf"), height = 9, dpi = 300)
    ggsave(paste0("~/Desktop/heatmap_", type, ".tiff"), height = 9, dpi = 300)
    
    #rm(list = ls())
