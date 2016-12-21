source('~/Dropbox/CU2016/F16CLASSES/BIG_Ching_Yang_Lin/nyctaxi/R/advent.R', echo=TRUE)
univUWS <- univ1month %>% filter( inUWS(px2, py2) ) %>%
    group_by(pickup_datetime, dropoff_datetime) %>% dplyr::slice(1) %>% group_by()

#master <- univUWS[1:10000, ]
master <- univUWS %>% sample_n(50000)

ggplot(master) + geom_point(aes(px,py, color=cut(hpay,6)))

tiny <- univUWS %>% filter(min >= 2)%>%  arrange(desc(hpay)) %>% head(10000)
slim <- master %>% select(-contains('id'), -contains('itude'), -improvement_surcharge, -store_and_fwd_flag, -passenger_count, -extra, -ehail_fee, -tolls_amount, -trip_type)

ggplot(slim, aes(x=trip_distance,y=as.numeric(min))) + geom_point(color="gray50", data=slim) + geom_point(aes(color=wday)) + facet_wrap( ~ wday )

mylinetype <- 2
png("EDA/univUWS-saw.png", width=1920, height=960)
    univUWS %>% mutate( t = day(pickup_datetime)*24+h ) %>% ggplot() + 
        geom_histogram(aes(x=t,fill=as.factor(isHigh)), position="fill", binwidth = 1) +
        scale_x_continuous(breaks=seq(24,840,by=6)) + geom_hline(yintercept=.75, linetype=mylinetype) +
        geom_hline(yintercept=.25, linetype=mylinetype) +  geom_hline(yintercept = .5, linetype=mylinetype)
dev.off()

# 11 is wed
pdf("EDA/univUWS-monthly-isHigh-rates.pdf", width=12,height=9)
tgtday <- 3
for( tgtday in 1:30){
    thisUWS <- univUWS %>% filter( d == tgtday )
    tgtw <- thisUWS$wday[1]
    thisUWS$rateCtg <- factor(thisUWS$rateCtg, levels=levels(thisUWS$rateCtg)[order(levels(thisUWS$rateCtg), decreasing = TRUE)])
    thigUWSgrouped <- thisUWS %>% group_by(h) %>% summarize( hr = sum(isHigh)/n() ) %>% mutate( hrl = ifelse(max(hr)==hr,paste0("MAX\n",round(100*hr,0)),round(100*hr,0)) )
    p <- thisUWS %>% ggplot() + geom_histogram(aes(x=h,fill=rateCtg), color="white", position="fill", binwidth = 1) + geom_freqpoly(aes(x=h,color=as.factor(isHigh)), position="fill", size=4, binwidth=1) + geom_label(aes(x=h,y=hr,label=hrl), data=thigUWSgrouped) + scale_color_manual(values = c("#F57670","black")) + labs(title=paste0("Day: 05-", tgtday, " ",tgtw), color="Rate of \nisHigh == 1\n(below\nblack line)", fill = "Tip Rate Type") + scale_x_continuous(breaks=0:25)
    print(p)
}
dev.off()

# Why such lucrative?
pdf("EDA/univUWS-monthly-intervals.pdf", width=12,height=9)
tgtday <- 3
for( tgtday in 1:30){
    thisUWS <- univUWS %>% filter( d == tgtday )
    tgtw <- thisUWS$wday[1]
    maxh <- thisUWS %>% group_by(h) %>% summarize( hr = sum(isHigh)/n() )
    maxh <- which.max(maxh$hr)-1
    p <-
    thisUWS %>% filter( tgtday == tgtday & h == maxh ) %>% group_by(isHigh) %>% arrange(desc(pickup_datetime)) %>% group_by() %>%
        mutate( r = row_number(), isHigh = as.factor(isHigh), rateCtg = ifelse(rateCtg=="other","other",ifelse(rateCtg%in%c('20%','25%','30%'),"20/25/30%",">20%"))) %>%
    ggplot() + geom_segment(aes(x=tm,xend=tm+as.numeric(min),y=r,yend=r, color=rateCtg, linetype=isHigh)) +
    geom_text(aes(tm+1/2*(as.numeric(min)),y=r+.5, color=rateCtg, label=paste0(round(min,0),"min, $",fare_amount,"+$",round(tip_amount,2)))) +
    scale_x_continuous(breaks=seq(0,2600,by=5)) +
    labs(title=paste0("Day: 05-", tgtday, " ",tgtw, " Hour:", maxh),
         subtitle="(trip minutes) $(fare_amount)+$(tip_amount)",
         color="Rate of \nisHigh == 1\n(below\nblack line)", fill = "Tip Rate Type")
    print(p)
}
dev.off()

library(units)
library(udunits2)
library(ggforce)
mysc <- scale_x_continuous(breaks=seq(0,1440,by=60))

pdf("EDA/univUWS-monthly-rates.pdf", width=12, height=9)
tgtday <- 15
for( tgtday in 1:30){
    p <- univUWS %>% filter( d == tgtday & hpay < 100 & rate <= 1 ) %>% ggplot(aes(tm,rate)) + geom_point(alpha=.2) + mysc + coord_flip()
    print(p)
}
dev.off()

pdf("EDA/univUWS-monthly-rates-box.pdf", width=12, height=9)
tgtday <- 15
for( tgtday in 1:30){
    p <- univUWS %>% filter( d == tgtday & hpay < 100 & rate <= .8 ) %>% ggplot(aes(h,rate, group=h)) + geom_boxplot(alpha=.2) + mysc + coord_flip()
    print(p)
}
dev.off()


pdf("EDA/univUWS-monthly-rates-box.pdf", width=12, height=9)
tgtday <- 15
for( tgtday in 1:30){
    p <- univUWS %>% filter( d == tgtday & hpay < 100 & rate <= .8 ) %>% ggplot(aes(h,rate, group=h)) + geom_boxplot(alpha=.2) + mysc + coord_flip()
    print(p)
}
dev.off()

univUWS %>% filter( hpay < 100 & rate <= .6 & isGenerous ) %>% ggplot(aes(rate, group=h3)) + geom_histogram(aes(fill=as.factor(h3)), position="fill") + scale_x_continuous(breaks=seq(.2,.8,by=.05))



univUWS %>% group_by(px3,py3) %>% ggplot() +
    ggplot(grouped) + geom_rect(aes(xmin=px4-sz, ymin=py4-sz,
                                    xmax=px4+sz, ymax=py4+sz,fill=cut(highRate,4) ))

univUWS %>% filter( hpay < 100 & rate <= .6 & isGenerous ) %>% ggplot(aes(h)) + geom_histogram(aes(fill=cut(as.numeric(min),6) ), binwidth = 1, position="fill")


master %>% filter( hpay < 100 & rate <= .6  )  %>% ggplot() + geom_point(aes(rate,hpay, color=isHigh))
master %>% filter( hpay < 50 & rate <= .6  )  %>% ggplot() + geom_point(aes(min,hpay, color=consRate)) + scale_color_manual(values = c("blue","red","gray50"))
master %>% filter( hpay < 50 & rate <= .6  )  %>% ggplot() + geom_point(aes(min,hpay, color=rateType)) + scale_color_manual(values = c("blue","red","orange", "gray50"))

master %>% filter( hpay < 50 & rate <= .6  )  %>% ggplot() + geom_point(aes(min,fare_amount, color=rateType))

back <- master %>% select(min,trip_distance)

pdf("EDA/univUWS-mins-and-distance.pdf", width=12, height=9)
    p <-
        ggplot(master %>% filter( hpay < 50 & rate <= .6  ), aes(min,trip_distance)) +
        geom_point(data=back, color="gray80") +
        geom_point(aes(color=consRate)) + facet_wrap(  ~ timeslot )
    print(p)
dev.off()

pdf("EDA/univ1month-mins-and-distance.pdf", width=12, height=9)
p <-
    ggplot(univ1month %>% filter( hpay < 50 & rate <= .6  ) %>% sample_n(50000) %>%
           mutate(  isGenerous = ifelse(rateType=="other"&rate>.2,1,0),
                    rateCtg = ifelse(isGenerous,"(20%<)other",rateType) %>% factor,
                   consRate = ifelse(rateCtg=="other","other",
                              ifelse(rateCtg%in%c('20%','25%','30%'),"20/25/30%",">20%")),
           min = as.numeric(min), wd = ifelse(isWeekday, "Weekday", "Weekend"),
           timeslot = h2ts(h) ), aes(min,trip_distance)) +
    geom_point(data=back, color="gray80") +
    geom_point(aes(color=consRate)) + facet_wrap(  ~ timeslot )
print(p)
dev.off()

