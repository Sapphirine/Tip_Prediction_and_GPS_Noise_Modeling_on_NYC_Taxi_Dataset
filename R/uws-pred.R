set.seed(12345)
master_size <- 100000
master <- univUWS %>% sample_n(master_size) %>% mutate( over20 = as.numeric(rate >= .20) )
tr <- master[1:floor(master_size/2),]
te <- master[ceiling(master_size/2):master_size,]
mm  <- model.matrix( ~ tz:wd + pickup_longitude + pickup_latitude + px + py + extra + isHigh + 0 ,master)
trmm <- mm[1:floor(master_size/2),]
temm <- mm[ceiling(master_size/2):master_size,]

rg <- list()
objVar <- 'isHigh'
colmm <- colnames(mm)
config <-
list(
    #when_latlon_who = colmm[! grepl('p[xy]', colmm)],
    when_where_who  = colmm[! grepl('picku', colmm)],
    where           = c('px', 'py')
)
config <- sapply(config, function(x) unique(c(x,objVar)) )

for( i in seq_along(config) ) {
    if( i == 2 ) next;
    message(i, appendLF = FALSE)
    rg[[i]] <- ranger(data= trmm[, config[[i]]], dependent.variable.name = objVar,
                      num.trees=100, classification = TRUE, importance = "permutation")
    message(" Error: ", rg[[i]]$prediction.error)
}

#sapply(rg, function(x) x$variable.importance)
par(las=2, mar=c(4,12,1,1), mfrow=c(2,2))
#sapply(rg, function(x) barplot(x$variable.importance, horiz=TRUE))

# 1 Error: 0.40258 
# 2 Error: 0.40152 -> 0.40036
# 3 Error: 0.4824


startm <- model.matrix( ~ tz:wd + px + py + extra + isHigh +
                          tolls_amount + trip_distance + 0 , tr)

rgstart <- ranger(data= startm, dependent.variable.name = objVar,
                  num.trees = 100, classification = TRUE, importance = "permutation")
rgstart$prediction.error # OOB error rate: 27.8 % 

afterm <- model.matrix( ~ tz:wd + px + py + extra + isHigh +
                            tolls_amount + trip_distance +
                            dropoff_longitude + dropoff_latitude + fare_amount + min + 0 , tr)

rgafter <- ranger(data= afterm, dependent.variable.name = objVar,
                  num.trees = 100, classification = TRUE, importance = "permutation")
rgafter$prediction.error # OOB error rate: 20.1 % 

cheatm <- model.matrix( ~ tz:wd + px + py + extra + isHigh +
                            tolls_amount + trip_distance +
                            dropoff_longitude + dropoff_latitude + fare_amount + min +
                            hpay + isHigh + 0 , tr)

rgcheat <- ranger(data= cheatm, dependent.variable.name = objVar,
                  num.trees = 50, classification = TRUE, importance = "permutation")
rgcheat$prediction.error # OOB error rate: 0 % (because hpay >= .12 completely explains isHigh)

sum(master$isHigh) / length(master$isHigh) # random guess error: 49.7 %

png("EDA/random-forests-varImp.png", height=960, width=1200)
    par(las=2, mar=c(4,12,3,1), mfrow=c(2,2))
    barplot(rg[[3]]$variable.importance, horiz=TRUE, main ="RndFst( hpayOver12Doller ~ features ) (OOB err = .48) (before, xy-only)\n (Note: Random Guess Error Rate: 49.7%)")
    barplot(rg[[2]]$variable.importance, horiz=TRUE, main ="RndFst (OOB err = .41) (before, xy&wday&h)")
    barplot(rgafter$variable.importance, horiz=TRUE, main ="RndFst (OOB err = .20) (After)")
    barplot(rgcheat$variable.importance, horiz=TRUE, main ="RndFst (OOB err = .00) (Cheat)")
dev.off()

tipm <- model.matrix( ~ tz:wd + pickup_longitude + pickup_latitude + px + py + passenger_count +
                        rid + vendor_id + cab_type_id + store_and_fwd_flag + dropoff_longitude + dropoff_latitude + 
                        trip_distance + fare_amount + over20 + min + 0 ,master)

rgtip <- ranger(data= tipm, dependent.variable.name = "over20",
                  num.trees = 60, classification = TRUE, importance = "permutation")
# OOB preiction error 28.27 %. Coincides with previous paper's result
barplot(rgtip$variable.importance, horiz=TRUE)

showSummary <- function(acc, pred) {
    ttt <- table(acc, pred)
    err_rate <- (ttt[1,2] + ttt[2,1]) / sum(ttt)
    print(ttt)
    message(" err rate:", err_rate)
}

showSummary(trmm[,objVar], rg[[2]]$predictions)
showSummary(trmm[,objVar], rg[[3]]$predictions)
showSummary(trmm[,objVar], rgafter$predictions)

showSummary(temm[,objVar], predict(rg[[2]], temm)$predictions)

tr$couldCorrect <- ((rgafter$predictions == trmm[,'isHigh']) & (rg[[2]]$predictions != trmm[,'isHigh']))

ggplot(tr %>% filter( hpay < 100 & rate <= .8 ) ) + geom_point(aes(rate, hpay, color=couldCorrect))
ggplot(tr %>% filter( hpay < 100 & rate <= .8 ) ) + geom_point(aes(trip_distance, hpay, color=couldCorrect)) # MAYBE-LATER interesting
ggplot(tr %>% filter( hpay < 100 & rate <= .8 ) ) + geom_point(aes(min, hpay, color=couldCorrect))
ggplot(tr %>% filter( hpay < 100 & rate <= .8 ) ) + geom_point(aes(fare_amount, hpay, color=couldCorrect))

table(trmm[, 'isHigh'])

tr %>% filter( couldCorrect & hpay < 12 ) %>% select( tip_amount, min, rate, h, wd, px, py )  %>% group_by(h, wd) %>% summarize(n=n())

tr %>% filter( couldCorrect & hpay < 12 ) %>% select( tip_amount, min, rate, h, wd, px, py )  %>% group_by(h, wd) %>% summarize(n=n()) %>% group_by() %>% arrange(desc(n)) %>% ggplot(aes(h,n)) + geom_line(aes(color=wd)) + geom_point()

source('R/pairs-extension.R')
png("EDA/univUWS-couldCorrect.png", width=1920,height=1240)
    pairs(tr %>% filter( couldCorrect & hpay < 12 ) %>% select( tip_amount, min, rate, h, px, py ),  lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist)
dev.off()

# Vendor difference
# inspired by: http://iquantny.tumblr.com/post/107245431809/how-software-in-half-of-nyc-cabs-generates-52

vendor_preprocess <- function(df,n=30000, lower=.196, upper=.2025) df[, c('vendor_id', 'fare_amount', 'mta_tax', 'improvement_surcharge', 'tolls_amount', 'extra', 'tip_amount', 'total_amount', 'rate')] %>% rename(surcharge = improvement_surcharge, fare = fare_amount, toll = tolls_amount, total = total_amount, vid = vendor_id, tax = mta_tax, tip = tip_amount) %>% arrange(vid) %>% mutate( op1 = (fare + surcharge) * rate, op2 = (fare + surcharge + toll + tax + extra) * rate ) %>% mutate( isop2 = op2 == tip, record_index = row_number(), vendor = ifelse(vid==1,"CMT","VFI") ) %>% filter( lower < rate & upper > rate) %>% sample_n(n) 

#vendordf <- univ1month
vendordf_20 <- vendor_preprocess(univ1month)
vendordf_25 <- vendor_preprocess(univ1month, lower = .243, upper=.2525)
vendordf_30 <- vendor_preprocess(univ1month, lower = .289, upper=.301)

pdf("EDA/univ1month-vendor-difference.pdf", width=15, height=12)
    vendordf_20 %>% ggplot() + geom_point(aes(rate,record_index,color=vendor),alpha=.3) + scale_x_continuous(breaks=seq(.196,.2025,by=.0005))
    vendordf_25 %>% ggplot() + geom_point(aes(rate,record_index,color=vendor),alpha=.3) + scale_x_continuous(breaks=seq(.243,.2525,by=.0005))
    vendordf_30 %>% ggplot() + geom_point(aes(rate,record_index,color=vendor),alpha=.3) + scale_x_continuous(breaks=seq(.289,.301, by=.0005))
dev.off()

# sensible criteria are:
# [.1975, .2025]
# [.2440, .2510]
# [.2945, .30025]