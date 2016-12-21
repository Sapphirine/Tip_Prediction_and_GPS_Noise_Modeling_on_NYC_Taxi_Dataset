

apply_ranger_univ061319 <- function(
  heavy,
  vars = c('h', 'hpay', 'wellPaid')
){

  ts <- substr(gsub(' |:|-', '', now()),1,20)
  set.seed(1234)

  rfmaster <-
    heavy %>%
    dplyr::select(cab_type_id, vendor_id, store_and_fwd_flag, 
                  rate_code_id, passenger_count, extra, # pickup_nyct2010_gid,
                  hpay, h, po, pa, wellPaid, wday) %>%
    mutate( vendor_id = as.numeric(as.factor(vendor_id)) -1 ,
            store_and_fwd_flag = as.numeric(as.factor(store_and_fwd_flag)) - 1 ) %>% sample_frac(.1)

  rangered <- ranger(data = data.frame(rfmaster[, vars]),
                     num.trees = 300, mtry = 2, min.node.size = 1,
                     scale.permutation.importance = TRUE,
                     importance = "permutation", write.forest = FALSE,
                     save.memory = FALSE, classification = FALSE, 
                     dependent.variable.name = 'hpay', seed=1234)
  f <- paste0('data/r', ts)
  save(rangered, f)
  message('saved to: ', f)
}

take2 <- function(){
  
  load('R/trips.20160613_19.all.RData')
  set.seed(1234)
  heavy <-
    univWeek %>% sample_frac(.1) %>%
    filter(-74.05 < pickup_longitude & pickup_longitude < -73.6 &
             40.50 < pickup_latitude  & pickup_latitude  < 41) %>% # remove seemingly incorrect records
    mutate( min = difftime(dropoff_datetime,pickup_datetime,units='mins'),
            hpay = tip_amount*60/as.numeric(min),
            isPremium = 12 * tip_amount - hpay < 0,
            #rate = tip_amount / total_amount,
            rate = tip_amount / (total_amount - tip_amount),
            is20 = ifelse(.199999 < rate & rate < .200001, TRUE, FALSE),
            is25 = ifelse(.249999 < rate & rate < .250001, TRUE, FALSE),
            is30 = ifelse(.299999 < rate & rate < .300001, TRUE, FALSE)
    ) %>%
    filter( payment_type == '1' & 2 < min & min < 120) %>%
    mutate( h = hour(pickup_datetime) ) %>%
    rename( tip = tip_amount ) %>%
    mutate( isHigh = isHigh(rate), po = round(pickup_longitude,3), pa = round(pickup_latitude,3) ) %>%
    group_by(po, pa) %>%
    mutate( n = n(), wellPaid = sum(isHigh)/ n, wday = wday(pickup_datetime) )
  
  rfmaster <- heavy %>% dplyr::select(cab_type_id, vendor_id, store_and_fwd_flag, 
                                      rate_code_id, passenger_count, extra, # pickup_nyct2010_gid,
                                      hpay, h, po, pa, wellPaid, wday) %>% mutate( vendor_id = as.numeric(as.factor(vendor_id)) -1 , store_and_fwd_flag = as.numeric(as.factor(store_and_fwd_flag)) - 1 ) %>% sample_frac(.1)
  
  rangered <- ranger(data = data.frame(rfmaster), num.trees = 300, mtry = round(sqrt(ncol(rfmaster)),0), min.node.size = 1,
                     scale.permutation.importance = TRUE, importance = "permutation", write.forest = FALSE,
                     save.memory = FALSE, dependent.variable.name = 'hpay', classification = FALSE, seed=1234)
  
  rangered <- ranger(data = data.frame(rfmaster[, c('po', 'pa', 'h', 'hpay') ]), num.trees = 3000, mtry = 2, min.node.size = 1,
                     scale.permutation.importance = TRUE, importance = "permutation", write.forest = FALSE,
                     save.memory = FALSE, dependent.variable.name = 'hpay', classification = FALSE, seed=1234)
  
  rangered2 <- ranger(data = data.frame(rfmaster[, c('pickup_longitude', 'pickup_latitude', 'h', 'hpay') ]), num.trees = 300, mtry = 2, min.node.size = 1,
                     scale.permutation.importance = TRUE, importance = "permutation", write.forest = FALSE,
                     save.memory = FALSE, dependent.variable.name = 'hpay', classification = FALSE, seed=1234)
  
  plot(rfmaster$hpay, rangered500$predictions)
  

}

take1 <- function(){
  library(ranger)
  
  rfmaster <- light %>% dplyr::select(cab_type_id, vendor_id, store_and_fwd_flag, rate_code_id, passenger_count, trip_distance, fare_amount, extra, hpay, h2)
  
  rangered <- ranger(data = rfmaster, num.trees = 1000, mtry = round(sqrt(ncol(rfmaster)),0), min.node.size = 1, scale.permutation.importance = TRUE, importance = "permutation", write.forest = TRUE, save.memory = FALSE, dependent.variable.name = 'hpay', classification = FALSE, seed=1234)
  
  rangered
  
  rangered$variable.importance
  
  
  
  rfmaster2 <- light %>% dplyr::select(cab_type_id, vendor_id, store_and_fwd_flag, rate_code_id, passenger_count, trip_distance, fare_amount, extra, tip, h2)
  
  rangered2 <- ranger(data = rfmaster2, num.trees = 1000, mtry = round(sqrt(ncol(rfmaster2)),0), min.node.size = 1, scale.permutation.importance = TRUE, importance = "permutation", write.forest = TRUE, save.memory = FALSE, dependent.variable.name = 'tip', classification = FALSE, seed=1234)
  
  rangered2
  
  
  plot(sort(rfmaster2$tip))
  plot(sort(log2(rfmaster$hpay)))
  
  
  tip / (dropoff_time - pickup_time)  
}
