
load('data/trips.20160613_19.all.RData')

master <- univWeek %>% sample_n(50000) %>% mutate(rate = tip_amount / (total_amount - tip_amount))

png("EDA/univWeek-distance-vs-tip-rate.png", width=960,height=960)
    ggplot(master %>% filter(rate<.4 & trip_distance < 20), aes(x=trip_distance,y=rate)) +
    geom_point(alpha=.5) +
    geom_smooth(method='lm')
dev.off()
