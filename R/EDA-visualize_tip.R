load(file="data/trips.20160613_19.all.RData")

univ

grouped <- univ1month %>% group_by(px4,py4) %>% summarise( meantip = mean(tip_amount), meanhpay = mean(hpay), medianhpay=median(hpay), highRate= sum(isHigh)/ n(), n = n(), pa = min(pickup_latitude), po = min(pickup_longitude) ) %>% filter( n >= 200 ) %>% mutate( hpayType = ifelse(highRate>=.5, 'HIGH','OTHER'))

sz <- 125

myscale <- myscale <-  list(scale_y_continuous(breaks=-50000), scale_x_continuous(breaks=-50000))

png("EDA/univ1month-meantip-grid.png", width=480, height=960)
ggplot(grouped) + geom_rect(aes(xmin=px4-sz, ymin=py4-sz,
                                xmax=px4+sz, ymax=py4+sz, fill = meantip )) + myscale
dev.off()


ggplot(grouped %>% filter(meanhpay<50)) + geom_rect(aes(xmin=px4-sz, ymin=py4-sz,
                                xmax=px4+sz, ymax=py4+sz, fill = meanhpay>16 )) + myscale
# not informative

ggplot(grouped %>% filter(medianhpay<50)) + geom_rect(aes(xmin=px4-sz, ymin=py4-sz,
                                                        xmax=px4+sz, ymax=py4+sz, fill = medianhpay > 12 )) + myscale

#+  scale_fill_grey(start = .9, end=.1)