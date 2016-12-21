

univ1month <- univ1month %>% mutate( inUWS = inUWS(px, py) )

inUWSdf <- univ1month %>% filter( inUWS == 1 ) %>% sample_n(50000)
outUWSdf <- univ1month %>% filter( inUWS != 1 ) %>% sample_n(50000)

inUWSdf %>% summarize( highRate = sum(isHigh) / n() )
# highRate
# 1   0.5002
outUWSdf %>% summarize( highRate = sum(isHigh) / n() )
# highRate
# 1  0.44566
