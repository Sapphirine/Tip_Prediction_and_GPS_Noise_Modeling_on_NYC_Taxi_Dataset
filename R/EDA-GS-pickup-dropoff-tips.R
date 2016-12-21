tables <- as.vector(dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema='public'"))


for(tbl in tables){
  glimpse(dbGetQuery(con, paste("SELECT * FROM", tbl, " LIMIT 10")))
}

# Analaysis 1: Goldman Sachs Dropoff

dodf <- # dropoff data frame
  query(" select * from goldman_sachs_dropoffs limit 10000") %>%
  dplyr::select(-pickup, -dropoff) %>%
  mutate( t = ifelse(dropoff_latitude<40.7144, 'STREET', 'GOLDMAN')) %>% # arbitrary
  mutate( inMt  = inManhattan(pickup_longitude, pickup_latitude) )

ggplot(dodf) + geom_point(aes(x=dropoff_longitude, y=dropoff_latitude)) # NB: logitude should be x for nyc-taxi-data compatibility 

murray <- data_frame(lat=c(40.715198, 40.715447), lon=c(-74.013120, -74.015003)) %>% mutate(s=(first(lat)-last(lat)) / (first(lon)-last(lon)), i=lat - lon*s )
  west <- data_frame(lat=c(40.715273, 40.713930), lon=c(-74.013697, -74.013848)) %>% mutate(s=(first(lat)-last(lat)) / (first(lon)-last(lon)), i=lat - lon*s )
 vesey <- data_frame(lat=c(40.714006, 40.714436), lon=c(-74.014442, -74.015353)) %>% mutate(s=(first(lat)-last(lat)) / (first(lon)-last(lon)), i=lat - lon*s )
 neave <- data_frame(lat=c(40.715806, 40.714845), lon=c(-74.015405, -74.016161)) %>% mutate(s=(first(lat)-last(lat)) / (first(lon)-last(lon)), i=lat - lon*s )
 
streets <- list(geom_abline(slope=murray$s[1], intercept=murray$i[1], color="pink",      lwd=3),
                geom_abline(slope=  west$s[1], intercept=  west$i[1], color="skyblue",   lwd=3),
                geom_abline(slope= vesey$s[1], intercept= vesey$i[1], color="darkgreen", lwd=3),
                geom_abline(slope= neave$s[1], intercept= neave$i[1], color="orange",    lwd=3))

png("EDA/GS-dropoff-cluster.png", width=960, height=960)
  ggplot(dodf) + geom_point(aes(x=dropoff_longitude, y=dropoff_latitude, color=t), size=.5, alpha=.8) + streets + 
  xlim(-74.0162, -74.0135) + ylim(40.7138, 40.7156) + 
  labs(title="Goldman Sachs Tower (West 200) Dropoff Location", subtitle="Two clusters: GS Entrance and Street Intersection")
dev.off()

ggplot() + maplist$mtg  +
  geom_point(data=dodf, aes(x=pickup_longitude, y=pickup_latitude, color=t), size=.5)
# => Mostly in Manhattan

png("EDA/GS-pickup-location.png", width=960, height=960)
  dodf %>% filter( inMt ) %>%
  ggplot() + maplist$mtg +
  geom_point(aes(pickup_longitude, pickup_latitude, color=t), size=.4)
dev.off()

dodf %>% group_by(t, passenger_count) %>% summarize( n = n() ) %>% group_by(t) %>% mutate( perc = round(n/sum(n)*100,1) )

dodf %>% mutate( h = hour(pickup_datetime)) %>% ggplot() + geom_density(aes(x=h, color=t)) + scale_x_continuous(breaks=-1:24)
dodf %>% mutate( h = hour(dropoff_datetime)) %>% ggplot() + geom_density(aes(x=h, color=t)) + scale_x_continuous(breaks=-1:24)

png("EDA/GS-pickup-location.png", width=960, height=960)
  ggplot(dodf) + geom_histogram(aes(x=tip_amount, fill=t), position='fill') + labs(title='Tip Amount Histogram (n=10,000)')
dev.off()
  
png("EDA/GS-tip-histogram.png", width=960, height=960)
  # NB: set limit 100,000 
  ggplot(dodf%>%filter(tip_amount<25)) + geom_histogram(aes(x=tip_amount, fill=t), position='fill') + labs(title='Tip Amount Histogram (n=100,000)')
dev.off()

png("EDA/GS-tip-histogram-parallel.png", width=960, height=960)
  ggplot(dodf%>%filter(tip_amount<25)) + geom_histogram(aes(x=tip_amount, fill=t)) + labs(title='Tip Amount Histogram (n=100,000)') + facet_grid( t ~ .)
dev.off()
  
# Analysis 2. Goldman Sachs Pickup Location

pudf <-
  query(" select * from gs_pickup limit 10000") %>%
  dplyr::select(-pickup, -dropoff) %>%
  mutate( t = ifelse(pickup_latitude<40.7144, 'STREET', 'GOLDMAN')) %>%
  mutate( inMt  = inManhattan(dropoff_longitude, dropoff_latitude) )

# Two clusters outside of Manhattan: La guardia and JFK airports
ggplot(pudf) + geom_point(aes(x=dropoff_longitude, y=dropoff_latitude, color=t), size=.5)

png("EDA/GS-pickup-cluster.png", width=960, height=960)
  ggplot(pudf) + geom_point(aes(x=pickup_longitude, y=pickup_latitude, color=t), size=.5, alpha=.4) + streets + 
  xlim(-74.0162, -74.0135) + ylim(40.7138, 40.7156) + 
  labs(title="Goldman Sachs Tower (West 200) Pickup Location (n=5000)", subtitle="Two clusters: GS Entrance and Street Intersection")
dev.off()