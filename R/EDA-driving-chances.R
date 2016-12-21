# univ <-
#   #query(" select * from trips order by random() limit 1000") %>% # FIXME: SO SLOW
#   query(" select * from trips where id % 5000 = 1") %>%
#   dplyr::select(-pickup, -dropoff) %>%
#   mutate( inMt  = inManhattan(pickup_longitude, pickup_latitude) )

# save(univ, file="trips.1in5000.random.RData")

master <- univ %>% filter( as.POSIXct("2016-06-01") < pickup_datetime & pickup_datetime < as.POSIXct("2016-06-07") ) %>% mutate( pt = pickup_datetime, dt = dropoff_datetime) 

master %>% mutate( pt = 3600*hour(pt) + 60*minute(pt) + second(pt), len = dropoff_datetime - pickup_datetime) %>% head(2000) %>% arrange(pt) %>% mutate(r = row_number()) %>% ggplot() + geom_segment(aes(x=pt, xend=pt+len, y=r, yend=r)) 

#univ %>% summarize( min = min(pickup_datetime), max= max(pickup_datetime) ) 

# Weighted Interval Scheduling http://www.geeksforgeeks.org/weighted-job-scheduling-log-n-time/

sort((master %>% mutate( pt = 3600*hour(pt) + 60*minute(pt) + second(pt), len = dropoff_datetime - pickup_datetime) )$len,decreasing = TRUE)[1:5]


# Incorrect data: 

# 
# univ2 <-
#  query(" select * from trips WHERE CAST(pickup_datetime AS DATE) = '2016-06-07'") %>%  # NB: SUPER SLOW. takes > 2h 
#  dplyr::select(-pickup, -dropoff) %>%
#  mutate( inMt  = inManhattan(pickup_longitude, pickup_latitude) )
# save(univ2, file="trips.20160607.all.RData")

load(file = 'data/trips.20160607.all.RData')
caption <- "Pickup Date: 2016-06-07 00:00:00 - 23:59:59. Data Source: data/trips.20160607.all.RData"

heavy <-
  univ2 %>%
  filter(-74.05 < pickup_longitude & pickup_longitude < -73.6 &
          40.50 < pickup_latitude  & pickup_latitude  < 41) %>% # remove seemingly incorrect records
  mutate( min = difftime(dropoff_datetime,pickup_datetime,units='mins'),
          hpay = tip_amount*60/as.numeric(min),
          isPremium = 12 * tip_amount - hpay < 0 ) %>%
  filter( payment_type == '1' & 2 < min & min < 120) %>%
  mutate( h = hour(pickup_datetime), h2 = as.factor(round(h/2,0)),
          pt = 3600*hour(pickup_datetime) + 60*minute(pickup_datetime) + second(pickup_datetime) ) %>%
  rename( tip = tip_amount )


png("EDA/20160607-linearModel.png", width=1920, height=1920)
  lmed <- lm( hpay ~ tip, data=heavy )
  ggplot(heavy) + geom_point(aes(x=tip,y=hpay), alpha=.2, size=1.5) + geom_abline(intercept=lmed$coefficients[1], slope=lmed$coefficients[2], color='red') + labs(title='Linear Model on Scatterplot', caption=caption, x='Tip Amount ($)', y='Hourly Pay [$]') + theme_bw() + scale_y_continuous(breaks=seq(0,3000,by=50))
dev.off()

png("EDA/20160607-linearModel-Premium.png", width=1920, height=1920)
  ggplot(heavy[!premiumv,]) + geom_point(aes(x=tip,y=hpay), alpha=.2, size=1.5) + geom_point(aes(x=tip,y=hpay), data=heavy[premiumv,], alpha=1, size=1.5, color="red") + geom_abline(intercept=lmed$coefficients[1], slope=lmed$coefficients[2], color='red') + labs(title='Linear Model on Scatterplot', subtitle = 'Pickup Date: 2016-06-07 00:00:00 - 23:59:59', x='Tip Amount ($)', y='Hourly Pay [$]') + theme_bw() + scale_y_continuous(breaks=seq(0,3000,by=50))
dev.off()

sum(heavy[  premiumv, ]$tip)
sum(heavy[! premiumv, ]$tip)

# pixelize <- function(v,prec=3,mod=2) {
#   tmp <- round(v, digits = prec)
#   tmp + round( (mod - ( as.integer(tmp * 10^prec) %% mod )) / 10^prec, digits=prec)
# }
# prc <- 4
# md <- 2
#heavy %>% mutate( po = pixelize(pickup_longitude,prec=prc, mod=md), pa = pixelize(pickup_latitude,prec=prc, mod=md) ) %>% group_by(po, pa) %>% filter( n() > 10 ) %>% summarize( wellPaid = sum(isPremium) / n() ) %>% ggplot() + geom_rect(aes(xmin=po, ymin=pa, xmax=po+md*10^(-prc), ymax=pa+md*10^(-prc), fill=wellPaid)) + labs(title="The well-paid area map", caption=caption)

png("EDA/20160607-linearModel-Premium.png", width=1920, height=1920)
  ggplot(heavy[! premiumv, ]) +
  geom_point(aes(x=pickup_longitude, y=pickup_latitude), size=.5) +
  geom_point(aes(x=pickup_longitude, y=pickup_latitude), size=.3, data=heavy[premiumv, ], color="red") +
  labs(title='Linear Model on Scatterplot', subtitle = 'Pickup Date: 2016-06-07 00:00:00 - 23:59:59', x='Tip Amount ($)', y='Hourly Pay [$]') + theme_bw()
dev.off()

pixelized <-
  heavy %>% mutate( po = round(pickup_longitude,3), pa = round(pickup_latitude,3) ) %>% group_by(po, pa) %>%
  filter( n() > 10 ) %>% summarize( wellPaid = sum(isPremium) / n() )
  
png("EDA/20160607-wellpaid-area.png", width=1920, height=1920)
  ggplot(pixelized) + geom_rect(aes(xmin=po, ymin=pa, xmax=po+.001, ymax=pa+.001, fill=wellPaid)) +
    labs(title="The well-paid area map", caption=caption) + theme(legend.position = "bottom")
dev.off()

png("EDA/20160607-wellpaid-area-category.png", width=1920, height=1920)
  ggplot(pixelized) +
  geom_rect(aes(xmin=po, ymin=pa, xmax=po+.001, ymax=pa+.001,
                fill=cut(wellPaid,4, labels=c('very few (<.13)', 'few (<.26)', 'much ( <.53)', 'abundant (> .53)')) )) +
  labs(title="The well-paid area map (threashold: n() < 10 for each rect)", caption=caption) +
  scale_fill_grey(guide = guide_legend(title = "Ratio of Higher Hourly Pay"), start=0.9, end=0.2 ) + theme_bw() + theme(legend.position = "bottom")
dev.off()


png("EDA/20160607-cash-paid-area.png", width=1920, height=1920)

heavy_cash <-
  univ2 %>%
  filter(-74.05 < pickup_longitude & pickup_longitude < -73.6 &
           40.50 < pickup_latitude  & pickup_latitude  < 41) %>% # remove seemingly incorrect records
  mutate( min = difftime(dropoff_datetime,pickup_datetime,units='mins'),
          hpay = tip_amount*60/as.numeric(min),
          isPremium = 12 * tip_amount - hpay < 0 ) %>%
  filter(                     2 < min & min < 120) %>%
  mutate( h = hour(pickup_datetime), h2 = as.factor(round(h/2,0)),
          pt = 3600*hour(pickup_datetime) + 60*minute(pickup_datetime) + second(pickup_datetime) ) %>%
  rename( tip = tip_amount )

heavy_cash %>% mutate( po = round(pickup_longitude,3), pa = round(pickup_latitude,3) ) %>% group_by(po, pa) %>%
  filter( n() > 10 ) %>% summarize( cash = sum(payment_type==2) / n() ) %>%
  ggplot() + geom_rect(aes(xmin=po, ymin=pa, xmax=po+.001, ymax=pa+.001, fill=cut(cash,4, labels=c('very few (<.13)', 'few (<.26)', 'much ( <.53)', 'abundant (> .53)')) )) + labs(title="The cash-paid area map (threashold: n() < 10 for each rect)", caption=caption) + scale_fill_grey(guide = guide_legend(title = "Ratio of Cash Payment"), start=0.9, end=0.2 ) + theme_bw() + theme(legend.position = "bottom")
rm(heavy_cash)
dev.off()

#pixelized %>% filter( wellPaid >= .5)
# Source: local data frame [12 x 3]
# Groups: po [12]
# 
# po     pa  wellPaid
# <dbl>  <dbl>     <dbl>
# 1  -74.016 40.718 0.5000000
# 2  -74.006 40.716 0.5000000
# 3  -74.001 40.750 0.5000000
# 4  -73.996 40.728 0.5000000
# 5  -73.990 40.766 0.5000000
# 6  -73.984 40.664 0.5000000
# 7  -73.958 40.609 0.6666667
# 8  -73.954 40.783 0.5000000
# 9  -73.951 40.743 0.5555556
# 10 -73.947 40.782 0.5000000
# 11 -73.941 40.788 0.5000000
# 12 -73.896 40.746 0.5000000
# => Google My Map
# http://www.relatedrentals.com/apartment-rentals/new-york-city/tribeca-battery-park-city/tribeca-tower/available-apartments/1-bedroom-1-bath-24037

heavy %>% filter( round(pickup_latitude,3) == 40.716 & round(pickup_longitude,3) == -74.006 ) %>% select(pickup_latitude, pickup_longitude)
heavy %>% filter( round(pickup_latitude,3) == 40.716 & round(pickup_longitude,3) == -74.006 ) %>% ggplot() + geom_segment(aes(x=pickup_longitude,xend=dropoff_longitude,y=pickup_latitude,yend=pickup_longitude))

#heavy %>% mutate( perc = total_amount / tip ) %>% ggplot() + geom_point(aes(x=pickup_longitude, y=perc))
png("EDA/20160607-tip-ratio.png", width=960, height=960)
  heavy %>% mutate( tip_ratio =  tip / total_amount ) %>% ggplot() + geom_point(aes(x=total_amount, y=tip_ratio), alpha=.7) + scale_y_continuous(breaks=seq(0,1,by=.05))
dev.off()


png("EDA/20160607-tip-ratio-enlarged.png", width=960, height=960)
  load('R/trips.20160613_19.all.RData')
  tmp<- univWeek %>% filter( 0.1666 < tip_amount / total_amount & tip_amount / total_amount < 0.1667 )
  ggplot(tmp) + geom_point(aes(x=total_amount, y=tip_amount/total_amount)) + scale_y_continuous(breaks=seq(0.1666,0.1667,by=.00001))
  sort(table(tmp$tip_amount / tmp$total_amount),decreasing = TRUE)[1]
dev.off()
  
# residual analysis

master <- univ2 %>% mutate( min = difftime(dropoff_datetime,pickup_datetime,units='mins') ) %>% filter( payment_type == '1' & 2 < min & min < 120) %>% sample_n( size = 30000 ) %>% mutate( h = hour(pickup_datetime), h2 = as.factor(round(h/2,0)), pt = 3600*hour(pickup_datetime) + 60*minute(pickup_datetime) + second(pickup_datetime) ) %>% rename( tip = tip_amount )

summed <- master %>% group_by(h) %>% summarize( mean = mean(tip), sd = sd(tip), median = median(tip), max = max(tip), min = min(tip), n = n() )

png("EDA/TRIP-tip-time-series.png", width=960, height=960)
  summed %>% ggplot(aes(x=h)) + geom_text(aes(y=mean, label=n)) + geom_point(aes(y=median), color='red') + geom_errorbar(aes(ymax = mean + sd, ymin=mean - sd))
dev.off()
# 4 - 5 a.m. is sparse. low predict accuracy might result


png("EDA/TRIP-tip-density.png", width=960, height=960)
  ggplot(master) + geom_density(aes(x=log2(tip), color=h2), alpha=.4, lwd=1)
dev.off()
                                      
ggplot(master) + geom_point(aes(x=tip, y=len))

source('R/pairs-extension.R')

paired <- master %>% select( -starts_with('pickup'), -starts_with('dropoff'), -h, -payment_type, -trip_type, -ehail_fee ) %>% mutate( vendor_id = as.numeric(vendor_id), store_and_fwd_flag = as.numeric(as.factor(store_and_fwd_flag)), min = as.numeric(min), h2 = as.numeric(h2)) %>% mutate( hpay = tip*60/min ) %>% filter( !is.na(hpay) )  %>% mutate_each(funs(jitter), cab_type_id, vendor_id, store_and_fwd_flag, passenger_count, h2 )

png("EDA/TRIP-scatterplot-matrix.png", width=2560, height=1900)
  pairs(paired, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist)
dev.off()

library(plotly)

ggplot(paired %>% arrange(hpay) %>% mutate( r = row_number() ) ) + geom_point(aes(x=r, y=hpay), size=1); ggplotly()

ggplot(paired %>% arrange(hpay) %>% mutate( r = row_number() ) ) + geom_point(aes(x=r, y=log2(hpay)), size=1)


table(master$tip_amount, master$payment_type)

mean(paired$tip)
2.788412 * 8 # = $ 22.3


preped <- master %>% select( -starts_with('pickup'), -starts_with('dropoff'), -h, -payment_type, -trip_type, -ehail_fee ) %>% mutate( vendor_id = as.numeric(vendor_id), store_and_fwd_flag = as.numeric(as.factor(store_and_fwd_flag)), min = as.numeric(min), h2 = as.numeric(h2)) %>% mutate( hpay = tip*60/min ) %>% filter( !is.na(hpay) )

light <- preped %>% sample_n( size= 20000 )

png("EDA/TRIP-intervals.png", width=960, height=960)
  ggplot(light %>% arrange(pt) ) + geom_segment(aes(y=log2(hpay), yend=log2(hpay), x = pt/60, xend = pt/60 + min, color=as.factor(vendor_id) ), alpha=.6)
dev.off()

png("EDA/TRIP-intervals-cut1.png", width=2560, height=1900)
  ggplot(light %>% filter( hpay >= 1 ) )  + facet_grid( cut_number(min, n=5) ~ .) + geom_hline( aes(yintercept=3), color='red', lwd=.5, linetype="dotted") + geom_hline( aes(yintercept=4), color='green', lwd=.5, linetype="dotted")  + geom_hline( aes(yintercept=2), color='orange', lwd=.5, linetype="dotted") + geom_segment(aes(y=log2(hpay), yend=log2(hpay), x = pt/60, xend = pt/60 + min, color=tip ), alpha=.6) + scale_x_continuous(breaks=seq(0,1560,by=30)) + theme_bw()
dev.off()


plot(light$min, log2(light$hpay), pch=18)
