source('~/nyctaxi/R/advent.R')
init()
univWeek <-
query(" select * from trips WHERE CAST(pickup_datetime AS DATE) BETWEEN '2016-06-13' AND '2016-06-19'") %>%  # NB: SUPER SLOW. takes > 2h 
dplyr::select(-pickup, -dropoff) %>%
mutate( inMt  = inManhattan(pickup_longitude, pickup_latitude) )
save(univWeek, file="/home/PCUser/nyctaxi/R/trips.20160613_19.all.RData")
#query(" select * from (select * from trips limit 300) a WHERE CAST(a.pickup_datetime AS DATE) BETWEEN '2011-06-13' AND '2016-06-19'") %>%  # NB: SUPER SLOW. takes > 2h 
# load("/home/PCUser/nyctaxi/R/trips.20160613_19.all.RData")
