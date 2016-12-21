
 aggregated <-
     univ1month %>% group_by(px4,py4) %>%
     summarise( highRate= sum(isHigh)/ n(), n = n(),
                averageTip = mean(tip_amount),
                pa = min(pickup_latitude),
                po = min(pickup_longitude) ) %>%
     filter( n > 100 )
 
 sz <- 125
 
 mylab <- labs(title="Area with average tip >= $6 trip records exceeding the half")
 myscale <-  list(scale_y_continuous(breaks=seq(0,60000,by=1000)), scale_x_continuous(breaks=seq(-7000,5000,by=750)))
 png("EDA/univ1month-averageTipOver3-twoColor.png", width=960, height=1920)
 ggplot(aggregated) + geom_rect(aes(xmin=px4-sz, ymin=py4-sz,
                                 xmax=px4+sz, ymax=py4+sz,fill=as.factor(averageTip>=3))) + mylab + scale_fill_manual(guide =guide_legend(title="averageTip\n >= $3"), values = c("#142C42", "#5AB3F5") ) + myscale
 dev.off()
 
 png("EDA/univ1month-averageTipOver3-twoColor.png", width=960, height=1920)
 ggplot(aggregated) + geom_rect(aes(xmin=px4-sz, ymin=py4-sz,
                                    xmax=px4+sz, ymax=py4+sz,fill=as.factor(highRate>=.5))) + labs(title="Area with hpay>=12 trip records exceeding the half") + scale_fill_manual(guide =guide_legend(title=">50% trips\nhpay>=$12"), values = c("#142C42", "#5AB3F5") ) + myscale
 dev.off()
 
 
 
 