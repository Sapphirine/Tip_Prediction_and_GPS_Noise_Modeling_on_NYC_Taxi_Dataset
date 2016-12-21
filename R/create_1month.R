source('~/nyctaxi/R/advent.R')
init()

EPSG2908 <- list( lonmin = -74.2700, lonmax = -71.7500,
                  latmin =  40.4700, latmax =  41.3100,
                  lonoff =   978000, latoff =   190000, # offset for meter (confirmed by "eyeballs")
                  theta = -.503)

univ4months <- 
  query(" select * from trips WHERE CAST(pickup_datetime AS DATE) BETWEEN '2016-05-01' AND '2016-05-31'") %>%  # NB: SUPER SLOW. takes > 2h 
  dplyr::select(-pickup, -dropoff) %>%
  mutate( inMt  = inManhattan(pickup_longitude, pickup_latitude) )

save(univ4months, file="/home/PCUser/nyctaxi/R/trips.20160501_0531.all.RData")

canTrans <-
  univ4months %>%
  filter(
      EPSG2908$lonmin <  pickup_longitude &  pickup_longitude < EPSG2908$lonmax &
      EPSG2908$lonmin < dropoff_longitude & dropoff_longitude < EPSG2908$lonmax &
      EPSG2908$latmin <  pickup_latitude  &  pickup_latitude  < EPSG2908$latmax &
      EPSG2908$latmin < dropoff_latitude  & dropoff_latitude  < EPSG2908$latmax
  ) # removed 2% rows

pmetered <- latlon2meter(canTrans$pickup_longitude, canTrans$pickup_latitude)
dmetered <- latlon2meter(canTrans$dropoff_longitude, canTrans$dropoff_latitude)


univ4month_grouped <- canTrans %>%
  mutate( cab_type_id = as.factor(cab_type_id),
          pom = pmetered@coords[,'lon'] - EPSG2908$lonoff, # pickup longitude in meter (offsetted)
          pam = pmetered@coords[,'lat'] - EPSG2908$latoff, # pickup latitude in meter  (offsetted)
          dom = dmetered@coords[,'lon'] - EPSG2908$lonoff,
          dam = dmetered@coords[,'lat'] - EPSG2908$latoff,
          #py_dep = cos(theta) * pickup_latitude  - sin(theta) * pickup_longitude, # angles distorted
          #px_dep = sin(theta) * pickup_latitude  + cos(theta) * pickup_longitude, # angles distorted
          py = rotateManhattanY(pam,pom,EPSG2908$theta), px = rotateManhattanX(pam,pom,EPSG2908$theta),
          dy = rotateManhattanY(pam,pom,EPSG2908$theta), dx = rotateManhattanX(pam,pom,EPSG2908$theta),
          h = hour(pickup_datetime), h2 = floor(h/2), h3 = floor(h/3), wday = wday(pickup_datetime, label=TRUE),
          isWeekday = ifelse(wday %in% c('Sat','Sun'),0,1),
          rate = tip_amount / (total_amount - tip_amount),
          rateType = isConstant(rate), isCons = ifelse(rateType=='other', FALSE, TRUE),
          px3 = ceiling(px) - ceiling(px)%%100, py3 = ceiling(py) - ceiling(py)%%100,
          px2 = ceiling(px) - ceiling(px)%%50 , py2 =  ceiling(py) - ceiling(py)%%50,
          pid = as.factor(pickup_nyct2010_gid),
          rid = as.factor(rate_code_id)
  ) %>% 
  #filter( -74.1 < pickup_longitude & pickup_longitude < -73.925  ) %>%
  filter( fare_amount > 0 & ! is.na(pid) ) %>% # troublesome for calculating rate
  #filter( -7000 < px & px < 5000 & 0 < py & py < 700000 ) %>%
  filter( payment_type == 1 )

univ4month_grouped <- univ4month_grouped %>% left_join(., univ4month_grouped %>% group_by(px2, py2) %>% summarize( n = n(), consRate = sum(isCons) / n() ), by=c('px2','py2')) %>% select(pickup_latitude, pickup_longitude, px2, py2, n, consRate)

save(univ4month_grouped, file="/home/PCUser/nyctaxi/R/trips.20160501_0531.grouped.RData")

#query(" select * from (select * from trips limit 300) a WHERE CAST(a.pickup_datetime AS DATE) BETWEEN '2011-06-13' AND '2016-06-19'") %>%  # NB: SUPER SLOW. takes > 2h 
# load("/home/PCUser/nyctaxi/R/trips.20160613_19.all.RData")
