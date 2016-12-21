load(file = 'data/trips.20160607.all.RData')

caption <- "Pickup Date: 2016-06-07 00:00:00 - 23:59:59. Data Source: data/trips.20160607.all.RData"


heavy <-
  univ2 %>%
  filter(-74.05 < pickup_longitude & pickup_longitude < -73.6 &
           40.50 < pickup_latitude  & pickup_latitude  < 41) %>% # remove seemingly incorrect records
  mutate( min = difftime(dropoff_datetime,pickup_datetime,units='mins'),
          hpay = tip_amount*60/as.numeric(min),
          isPremium = 12 * tip_amount - hpay < 0,
          #rate = tip_amount / total_amount,
          rate = tip_amount / (total_amount - tip_amount),
          is16 = ifelse(.166666 < hpay & hpay < .166667, TRUE, FALSE),
          is20 = ifelse(.199999 < hpay & hpay < .200001, TRUE, FALSE)
          ) %>%
  filter( payment_type == '1' & 2 < min & min < 120) %>%
  mutate( h = hour(pickup_datetime), h2 = as.factor(round(h/2,0)),
          pt = 3600*hour(pickup_datetime) + 60*minute(pickup_datetime) + second(pickup_datetime) ) %>%
  rename( tip = tip_amount ) %>%
  mutate( isHigh = isHigh(rate), po = round(pickup_longitude,3), pa = round(pickup_latitude,3) )

png("EDA/20160607-constantTipRatio.png", width=960, height=960)
  tmp <- heavy %>% filter(rate < 1) %>% mutate( isInt = ifelse( round(rate*100,0) == rate*100, TRUE, FALSE ) )
  ggplot(tmp %>% filter(! isInt),aes(total_amount, rate) ) +
  geom_point(size=1.3, color="#888888") + geom_point(data=tmp %>% filter(isInt), color="red") + scale_y_continuous(breaks=seq(0,1,by=.05)) + labs(x='Total Payment [Dollar]', y='Tip Ratio in Total Payment', caption = caption)
  rm(tmp)
dev.off()
  
heavy %>% filter( isHigh ) %>% ggplot() + geom_point(aes(pickup_longitude,pickup_latitude))

pixelized <-
  heavy %>% group_by(po, pa) %>%
  filter( n() > 10 ) %>% summarize( wellPaid = sum(isHigh) / n() )

png("EDA/20160607-202530-tip-area.png", width=1920, height=1920)
  ggplot(pixelized) + geom_rect(aes(xmin=po, ymin=pa, xmax=po+.001, ymax=pa+.001, fill=wellPaid)) +
  labs(title="The well-paid area map", caption=caption) + theme(legend.position = "bottom")
dev.off()

png("EDA/20160607-202530-tip-area-category.png", width=1920, height=1920)
ggplot(pixelized) + geom_rect(aes(xmin=po, ymin=pa, xmax=po+.001, ymax=pa+.001, fill=cut(wellPaid, 3))) +
  labs(title="The well-paid area map", caption=caption) + theme(legend.position = "bottom")
dev.off()

heavy %>% group_by(po,pa) %>% mutate( wellPaid = sum(isHigh)/ n() ) %>% View

tmp <- heavy %>% group_by(po,pa) %>% mutate( n = n(), wellPaid = sum(isHigh)/ n )


tmp %>% filter( wellPaid > .7 & n > 10)

load('R/trips.20160613_19.all.RData')
heavy <-
  univWeek %>%
  filter(-74.05 < pickup_longitude & pickup_longitude < -73.6 &
           40.50 < pickup_latitude  & pickup_latitude  < 41) %>% # remove seemingly incorrect records
  mutate( min = difftime(dropoff_datetime,pickup_datetime,units='mins'),
          hpay = tip_amount*60/as.numeric(min),
          isPremium = 12 * tip_amount - hpay < 0,
          #rate = tip_amount / total_amount,
          rate = tip_amount / (total_amount - tip_amount),
          is16 = ifelse(.166666 < hpay & hpay < .166667, TRUE, FALSE),
          is20 = ifelse(.199999 < hpay & hpay < .200001, TRUE, FALSE)
  ) %>%
  filter( payment_type == '1' & 2 < min & min < 120) %>%
  mutate( h = hour(pickup_datetime), h2 = as.factor(round(h/2,0)),
          pt = 3600*hour(pickup_datetime) + 60*minute(pickup_datetime) + second(pickup_datetime) ) %>%
  rename( tip = tip_amount ) %>%
  mutate( isHigh = isHigh(rate), po = round(pickup_longitude,3), pa = round(pickup_latitude,3) )

tmp <- heavy %>% group_by(po,pa) %>% mutate( n = n(), wellPaid = sum(isHigh)/ n )

tmp %>% filter( wellPaid > .7 & n > 30)



tmp %>% filter( wellPaid > .65 & n > 30 ) %>% slice(1) %>% ggplot() + maplist$mtg + geom_point(aes(pickup_longitude, pickup_latitude), color="skyblue")

tmp %>% filter( wellPaid > .65 & n > 30 ) %>% select(pickup_datetime, dropoff_datetime, tip, total_amount, rate, wellPaid, tolls_amount, passenger_count) %>% View


tmp %>% filter( wellPaid > .65 & n > 30 ) %>% arrange(po) %>% select(po, pa,pickup_datetime, min, tip, hpay, total_amount, rate, wellPaid, tolls_amount, passenger_count) %>% View

tmp %>% filter( wellPaid > .75 & n > 30 ) %>% arrange(po) %>% select(po, pa,pickup_datetime, min, tip, hpay, total_amount, rate, wellPaid, tolls_amount, passenger_count) %>% slice(1)
