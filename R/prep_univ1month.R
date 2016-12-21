univ4months <-
    univ4months %>%
    filter(
            gcl$EPSG2908$lonmin <  pickup_longitude &  pickup_longitude < gcl$EPSG2908$lonmax &
            gcl$EPSG2908$lonmin < dropoff_longitude & dropoff_longitude < gcl$EPSG2908$lonmax &
            gcl$EPSG2908$latmin <  pickup_latitude  &  pickup_latitude  < gcl$EPSG2908$latmax &
            gcl$EPSG2908$latmin < dropoff_latitude  & dropoff_latitude  < gcl$EPSG2908$latmax
    ) # removed 2% rows

pmetered <- latlon2meter(univ4months$pickup_longitude, univ4months$pickup_latitude)
dmetered <- latlon2meter(univ4months$dropoff_longitude, univ4months$dropoff_latitude)

set.seed(1236)

univ4months <- univ4months %>%
    mutate( cab_type_id = as.factor(cab_type_id),
            pom = pmetered@coords[,'lon'], # pickup longitude in meter (offsetted)
            pam = pmetered@coords[,'lat'], # pickup latitude in meter  (offsetted)
            dom = dmetered@coords[,'lon'],
            dam = dmetered@coords[,'lat'],
            #py_dep = cos(theta) * pickup_latitude  - sin(theta) * pickup_longitude, # angles distorted
            #px_dep = sin(theta) * pickup_latitude  + cos(theta) * pickup_longitude, # angles distorted
            py = rotateManhattanY(pam,pom,gcl$EPSG2908$theta), px = rotateManhattanX(pam,pom,gcl$EPSG2908$theta),
            dy = rotateManhattanY(pam,pom,gcl$EPSG2908$theta), dx = rotateManhattanX(pam,pom,gcl$EPSG2908$theta),
            h = hour(pickup_datetime), h2 = floor(h/2), h3 = floor(h/3), wday = wday(pickup_datetime, label=TRUE),
            isWeekday = ifelse(wday %in% c('Sat','Sun'),0,1),
            rate = tip_amount / (total_amount - tip_amount),
            rateType = isConstant(rate), isCons = ifelse(rateType=='other', FALSE, TRUE),
            px4 = ceiling(px) - ceiling(px)%%250, py4 = ceiling(py) - ceiling(py)%%250,
            px3 = ceiling(px) - ceiling(px)%%100, py3 = ceiling(py) - ceiling(py)%%100,
            px2 = ceiling(px) - ceiling(px)%%50 , py2 =  ceiling(py) - ceiling(py)%%50,
            min = difftime(dropoff_datetime,pickup_datetime,units='mins'),
            hpay = tip_amount*60/as.numeric(min), isHigh = as.numeric(hpay >= 12),
            pid = as.factor(pickup_nyct2010_gid),
            rid = as.factor(rate_code_id),
            d = day(pickup_datetime), m = minute(pickup_datetime),
            tm = as.numeric(h)*60+as.numeric(m), isGenerous = ifelse(rateType=="other"&rate>.2,1,0),
            rateCtg = ifelse(isGenerous,"(20%<)other",rateType) %>% factor,
            consRate = ifelse(rateCtg=="other","other",ifelse(rateCtg%in%c('20%','25%','30%'),"20/25/30%",">20%")),
            min = as.numeric(min), wd = ifelse(isWeekday, "Weekday", "Weekend"),
            timeslot = h2ts(h), ts = gsub(' |\\]|^[ABCDE]|-','',h2ts(h)), tz = h2tz(h)
    ) %>% 
    filter( inMt & -74.1 < pickup_longitude & pickup_longitude < -73.925  ) %>%
    filter( fare_amount > 0 & ! is.na(pid) ) %>% # troublesome for calculating rate
    filter( -7000 < px & px < 5000 & 0 < py & py < 700000 ) %>%
    filter( 0 < min & min < 120 ) %>%
    filter( payment_type == 1 )

univ1month <- univ4months
univ1month <- univ1month %>% filter( 5000 < py4 & py4 < 60000)
save(univ1month, file = "data/trips.20160501_0531.preped.RData")
