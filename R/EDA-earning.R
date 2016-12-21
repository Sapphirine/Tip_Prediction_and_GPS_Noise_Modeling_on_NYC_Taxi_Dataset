# http://www.ehow.com/info_8496448_much-make-new-york-city.html
# says NYC taxi drivers earn $14.74 per hour on average.

#
# "Income for owners of independent medallions is derived from
# the fares and tips received from passengers less the
# cost of owning and maintaining a vehicle and medallion. "
#  http://www.nyc.gov/html/tlc/downloads/pdf/2014_taxicab_fact_book.pdf
# 

load(file = 'data/trips.20160613_19.all.RData')
library(tidyverse)
library(lubridate)
set.seed(1236)
theta <- -.632 # 

huge %>% group_by(day) %>% summarize_each(funs(min,mean,median,max), tip_amount, min)
huge %>% mutate( hfare=as.numeric(min)/fare_amount*60) %>% summarize_each(funs(mean,median,max), tip_amount, min, hfare)
huge %>% group_by(day) %>% filter( min == max(min)) # what's these 24h trips?

as.numeric(huge$min[1])/mini$fare_amount[1]*60

# => not promising

# street analysis

with(huge,plot(px,py,pch=18, cex=.3))
text(landmark$x, landmark$y, label="TSQ", col="red", cex=1.5)

with(huge%>%filter(in11thAve),plot(px,py, pch=18))

huge %>% filter(in11thAve) %>% mutate( t = day*8 + h3 ) %>% group_by(py3,t) %>% summarise( n = n(), tip = median(tip_amount), hpay= mean(hpay) ) %>% ggplot() + geom_rect(aes(xmin=t,xmax=t+1,ymin=py3-.0005,ymax=py3+.0005,fill=n)) + scale_y_continuous(breaks=seq(-10.85,-10.725,by=.001))

huge %>% filter( in11thAve & py3 == -10.84 ) %>% group_by(px4) %>% summarize( n = n() )
huge %>% filter( px4 == -83.7852 & py3 == -10.84)

maped <- get_map(c(-74.00562, 40.75076), source = c('google', 'osm', 'stamen', 'cloudmade')[1], zoom=16)
ggmap(maped) + geom_point(aes(pickup_longitude, pickup_latitude), data = huge %>% filter( in11thAve & py3 == -10.84 ))

nyc <- get_map(c(-73.98, 40.76076), source = c('google', 'osm', 'stamen', 'cloudmade')[1], zoom=14)
ggmap(nyc) + geom_point(aes(pickup_longitude,pickup_latitude), data=huge%>%filter(inTS)%>%sample_n(1000))


png("EDA/univWeek-constantRate-hpay.png", width=960, height=960)
    ggplot(mini%>%filter(hpay<80)) + geom_point(aes(x=tip_amount,y=hpay,color=rateType)) + facet_wrap(  ~ rateType )
dev.off()

ggplot(mini%>%filter(hpay<80)) + geom_point(aes(x=tip_amount,y=hpay,color=rateType,shape=rateType)) + facet_grid( cut(hpay,6) ~ .)  

huge %>% group_by(rateType) %>% summarise_each(funs(min,max,mean), hpay)
# rateType         min   max     mean   median
# (chr)       (dbl) (dbl)    (dbl)    (dbl)
# 1      20% -15.3236152 28800 14.60732 12.00000
# 2      25%   0.6411699 23760 19.09469 14.97453
# 3      30%   1.3485468 23148 23.86606 18.63035
# 4    other -75.7894737 72000 16.76719 10.51282
# 5       NA   0.0000000     0  0.00000  0.00000

with(mini %>% filter(rate < 1), plot(tip_amount, rate, col=as.factor(isHigh), pch=18))

library(ranger)
ed <- ranger(data = cbind(model.matrix( ~ vendor_id + pickup_longitude + pickup_latitude , mini), as.matrix(data.frame(isHigh=mini$isHigh))), dependent.variable.name = "isHigh", classification = TRUE, save.memory = FALSE, num.trees = 3000)
train(isHigh~vendor_id, data=mini, method="glm", family=binomial, trControl=trainControl("cv", 5, savePredictions=TRUE))

summary(glm( isHigh ~ consRate, data = mini ))

preped <- huge %>% group_by(px4,py4) %>% summarise( highRate= sum(isHigh)/ n(), n = n(), pa = min(pickup_latitude),po = min(pickup_longitude) ) %>% filter( n > 100 )

sz <- 125

mylab <- labs(title="Area with hpay>=12 trip records exceeding the half")
myscale <-  list(scale_y_continuous(breaks=seq(0,60000,by=1000)), scale_x_continuous(breaks=seq(-7000,5000,by=750)))
png("EDA/univWeek-hpayOver12-grid.png", width=960, height=1920)
    ggplot(preped) + geom_rect(aes(xmin=px4-sz, ymin=py4-sz,
                                                          xmax=px4+sz, ymax=py4+sz,fill=highRate)) + mylab + myscale
dev.off()

png("EDA/univWeek-hpayOver12-twoColor.png", width=960, height=1920)
ggplot(preped ) + geom_rect(aes(xmin=px4-sz, ymin=py4-sz,
                                                      xmax=px4+sz, ymax=py4+sz,fill=as.numeric(highRate>=.5))) + mylab + scale_fill_continuous(guide =guide_legend(title="highRate", reverse=1)) + myscale
dev.off()

# search upper west side area

# "Upper West Side is bounded on the south by 59th Street, Central Park to the east, and the Hudson River to the west. Its northern boundary is somewhat less obvious. Although it has historically been cited as 110th Street,[5] which fixes the neighborhood alongside Central Park, it is now sometimes (primarily by the real estate industry) considered to be 125th Street, encompassing Morningside Heights" (Wikipedia)
# Columbus Circle is in 59st street

#     geom_label_repel(aes(x,y,label=name), fill=fillcolor, landmark,
#point.padding = unit(1, "lines"), fontface = 'bold', color = 'white',
#max.iter = 100, nudge_x = -200, segment.color = fillcolor,
#segment.size =1.5)


fillcolor <- "#EC365E"
ggplot(preped %>% filter(34000<py&py<42000&-6500<px&px< -4500)) + geom_rect(aes(xmin=px4-sz, ymin=py4-sz,
                                                      xmax=px4+sz, ymax=py4+sz,fill=as.numeric(highRate>=.5))) +
    mylab + scale_fill_continuous(guide =guide_legend(title="highRate")) + myscale

maped <- get_map(c(-73.98193, 40.77825), source = c('google', 'osm', 'stamen', 'cloudmade')[1], zoom=16)
ggmap(maped) + geom_point(aes(po, pa), data = preped %>% filter(34000<py4&py4<42000& px4 == -6000 )  )



load(file = "data/trips.20160501_0531.preped.RData")

grouped <- univ1month %>% group_by(px4,py4) %>% summarise( highRate= sum(isHigh)/ n(), n = n(), pa = min(pickup_latitude),po = min(pickup_longitude) ) %>% filter( n >= 200 ) %>% mutate( hpayType = ifelse(highRate>=.5, 'HIGH','OTHER'))


ggplot(grouped) + geom_rect(aes(xmin=px4-sz, ymin=py4-sz,
                                xmax=px4+sz, ymax=py4+sz,fill=cut(highRate,4) )) + mylab + myscale + scale_fill_grey(start = .9, end=.1)


png("EDA/univ1month-hpayOver12-grid.png", width=960, height=1920)
    ggplot(grouped) + geom_rect(aes(xmin=px4-sz, ymin=py4-sz,
                                    xmax=px4+sz, ymax=py4+sz,fill=highRate)) + mylab + myscale
dev.off()

png("EDA/univ1month-hpayOver12-twoColor.png", width=960, height=1920)
    ggplot(grouped) + geom_rect(aes(xmin=px4-sz, ymin=py4-sz,
                                     xmax=px4+sz, ymax=py4+sz,fill=hpayType)) +
        mylab + myscale + scale_fill_manual(values=c('#5DB6F9','#000000'))
dev.off()
