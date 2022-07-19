# extract lower bound of yearly from annual seasonal component
d <- grep("p",names(.GlobalEnv),value=TRUE)

type <- sub(".*\\.", "", d)[1]
type <- substr(type, start=1, stop = nchar(type)-1)

d <- d[!d %in% c("type", "country.lookup")] # doesnt exactly work but this does. 

# # keep text after last periods
# type <- sub(".*\\.", "", rownames(d))[1]
# type <- substr(type, start=1, stop = nchar(type)-1)

    # turn d to data frame
    d <- do.call("list",mget(d))
    d <-  data.frame("rate" = unlist(lapply(d,function(x) x$data[,8])))
    
    library(stringi)
    # keep first numbers
    d$test <- as.numeric(stri_extract_first_regex(rownames(d), "[0-9]+"))
    library(stringr)
    # keep text between periods
    d$country <- stringr::str_extract(string = rownames(d), pattern = "(?<=\\.).*?(?=\\.)")
    # country lookup
    country.lookup <- data.frame("country" = c(eu.countries, "United States", "Canada"))
    country.lookup$number <- c(seq(1:length(eu.countries)), 1, 1)
    
    d %>%
      filter(
        country == "eu"
      ) %>%
      arrange(
        test
      ) -> d.eu
    d.eu$country <- rep(eu.countries, each=365)
     
    d %>%
      filter(
        country %in% c("us")
      ) %>%
      arrange(
        test
      ) -> d.us
    d.us$country <- rep(c("United States"), each=365)
    
    d <- data.frame(rbind(d.eu, d.us))
    
    
    # add country name 
    
    d$date <- rep(seq.Date(as.Date("2020-01-01"), by="day", length.out=365), times=29)
    d %>%
      group_by(country, lubridate::month(date)) %>%
      summarise(
        med.rate = median(rate, na.rm=T),
        #country = country
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
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", breaks = c(minz,  maxz), labels = c("Low", "High")) + 
      scale_x_continuous(breaks = c(seq(1:12)), labels = c(month.abb)) + 
      labs(
        x = "",
        y = "",
        fill = "Seasonal Impact on \nRate Per 100,000"
      ) +
      theme(
        panel.background = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5)
      ) 
    
    ggsave(paste0("~/Desktop/", type, ".pdf"), height = 12)
    
    #rm(list = ls())
