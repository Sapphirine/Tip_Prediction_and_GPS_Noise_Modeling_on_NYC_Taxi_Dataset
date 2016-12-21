load(file = 'data/trips.20160613_19.all.RData')
library(tidyverse)
library(lubridate)

# "Before TPEP systems were introduced to all yellow taxis
# in late 2008, no taxis accepted credit cards. At the
# time when credit card readers had been installed in all
# taxis in late 2008, passengers paid by credit card for
# less than 20% of all trips, and the share of trips paid by
# credit card has grown steadily since then. Today, paying
# with a credit card is more popular than paying with
# cash, as 55% of all trips are paid by credit card."
#   http://www.nyc.gov/html/tlc/downloads/pdf/2014_taxicab_fact_book.pdf

library(rgdal)

# translation bounds From: http://www.spatialreference.org/ref/epsg/2908/
EPSG2908 <- list( lonmin = -74.2700, lonmax = -71.7500,
                  latmin =  40.4700, latmax =  41.3100,
                  lonoff =   978000, latoff =   190000, # offset for meter (confirmed by "eyeballs")
                  theta = -.503)

canTrans <-
    univWeek %>%
    filter(
        EPSG2908$lonmin <  pickup_longitude &  pickup_longitude < EPSG2908$lonmax &
        EPSG2908$lonmin < dropoff_longitude & dropoff_longitude < EPSG2908$lonmax &
        EPSG2908$latmin <  pickup_latitude  &  pickup_latitude  < EPSG2908$latmax &
        EPSG2908$latmin < dropoff_latitude  & dropoff_latitude  < EPSG2908$latmax
    ) # removed 2% rows

latlon2meter <- function(lon, lat)
    spTransform(SpatialPoints(cbind(lon,lat), proj4string=CRS("+ellps=WGS84 +datum=WGS84 +proj=longlat")),
                CRS("+init=epsg:2908"))
rotateManhattanY <- function(latm, lonm, theta)  cos(theta) * latm  - sin(theta) * lonm
rotateManhattanX <- function(latm, lonm, theta)  sin(theta) * latm  + cos(theta) * lonm

pmetered <- latlon2meter(canTrans$pickup_longitude, canTrans$pickup_latitude)
dmetered <- latlon2meter(canTrans$dropoff_longitude, canTrans$dropoff_latitude)

set.seed(1236)

huge <- canTrans %>%
    mutate( pom = pmetered@coords[,'lon'] - EPSG2908$lonoff, # pickup longitude in meter (offsetted)
            pam = pmetered@coords[,'lat'] - EPSG2908$latoff, # pickup latitude in meter  (offsetted)
            dom = dmetered@coords[,'lon'] - EPSG2908$lonoff,
            dam = dmetered@coords[,'lat'] - EPSG2908$latoff,
            #py_dep = cos(theta) * pickup_latitude  - sin(theta) * pickup_longitude, # angles distorted
            #px_dep = sin(theta) * pickup_latitude  + cos(theta) * pickup_longitude, # angles distorted
            py = rotateManhattanY(pam,pom,EPSG2908$theta), px = rotateManhattanX(pam,pom,EPSG2908$theta),
            dy = rotateManhattanY(pam,pom,EPSG2908$theta), dx = rotateManhattanX(pam,pom,EPSG2908$theta),
            h = hour(pickup_datetime) ) %>% 
    filter( inMt & -74.1 < pickup_longitude & pickup_longitude < -73.925  ) %>%
    filter( -7000 < px & px < 5000 & 0 < py & py < 700000 ) %>%
    filter( payment_type == 1 ) %>%
    sample_n(3000000)

plot(huge$px[1:100000], huge$py[1:100000], pch=18, cex=.8)

test <- huge[1:100000, ]
mini <- huge[  100001:200000, ]

if (FALSE) {
    rotate_ny <- function(mini, theta = -.632, mode = c('meter', 'latlong')[1] ) {
        if ( mode == 'meter' ){ message("mode: meter")
            mini$y <- cos(theta) * mini$pam - sin(theta) * mini$pom
            mini$x <- sin(theta) * mini$pam + cos(theta) * mini$pom
            plot(mini$x, mini$y, pch=18, cex=.6)
        } else { message("mode: latlong")
            # invalid transformation (angles will be distorted)
            mini$y <- cos(theta) * mini$pickup_latitude - sin(theta) * mini$pickup_longitude
            mini$x <- sin(theta) * mini$pickup_latitude + cos(theta) * mini$pickup_longitude
            plot(mini$x, mini$y, pch=18)
        }
    }
    rotate_ny(huge[1:50000,], -.508)
    rotate_ny(huge[1:10000,], -.632) ; abline(a=-83.90, b=0,col="red")
    abline(v=-83.78,col="red")

    # FIXME origin
    landmark <- data_frame( name = c('TSQ', '51ST', 'CU', 'Julliard', 'CCircle'),
                            lat = c( 40.758855,  40.757002,  40.807933,  40.773928,  40.768077),
                            lon = c(-73.985134, -73.972018, -73.963741, -73.982824, -73.981886) ) %>% 
        mutate( y = cos(theta) * lat - sin(theta) * lon,
                x = sin(theta) * lat + cos(theta) * lon )
    
    png("EDA/univWeek-GPS-accuracy.png", width=1200, height=2400)
        plot(huge$px, huge$py, pch=18, xlab="", ylab="", cex=.5, 
             main="Weekly Pickup Location in Manhattan \n(n=3,000,000)", axes=FALSE)
        text(landmark$x, landmark$y, label=landmark$name, col="white", cex=1.5)
    dev.off()
}
  
# Useful: https://www.google.com/maps/@40.7635299,-73.9162908,724a,20y,270h,83.12t/data=!3m1!1e3?hl=en


tiny <- mini %>% #filter( -83.8 < px & px < -83.745 & -10.9 < py & py < -10.725 ) %>%
    mutate( px3 = round(px,3), py3 = round(py,3),
            px4 = round(px,4), py4 = round(py,4))

library(rpart)
library(rpart.plot)
#library(plotmo) # too heavy

s <- 50
ctl <- rpart.control(minsplit=s,minbucket=3,cp=0,maxsurrogate=0,maxcompete=0)
model_raw <- rpart( tip_amount ~ ., data=tiny %>% select(tip_amount, pickup_longitude, pickup_latitude), control=ctl)
model_xy  <- rpart( tip_amount ~ ., data=tiny %>% select(tip_amount,              px ,             py ), control=ctl)
model_xy3 <- rpart( tip_amount ~ ., data=tiny %>% select(tip_amount,              px3,             py3), control=ctl)
model_xy4 <- rpart( tip_amount ~ ., data=tiny %>% select(tip_amount,              px3,             py4), control=ctl)
model_xy44<- rpart( tip_amount ~ ., data=tiny %>% select(tip_amount,              px4,             py4), control=ctl)
model_xy_4<- rpart( tip_amount ~ ., data=tiny %>% select(tip_amount,              px ,             py4), control=ctl)
model_xy4_<- rpart( tip_amount ~ ., data=tiny %>% select(tip_amount,              px4,             py ), control=ctl)
model_xyd <- rpart( tip_amount ~ ., data=tiny %>% select(tip_amount,trip_distance,px,              py ), control=ctl)
model_xyh <- rpart( tip_amount ~ ., data=tiny %>% select(tip_amount, h,           px,              py ), control=ctl)

if (FALSE) {
    xrange <- sort(unique(tiny$px3))
    yrange <- sort(unique(tiny$py3))
    xd <- (xrange[2]-xrange[1])*.5 # x dodge
    yd <- (yrange[2]-yrange[1])*.5
    testdata <- expand.grid( px = xrange, py = yrange)
    out <- bind_cols( testdata, data_frame(pred = predict(model,testdata)))
    ggplot(out) + geom_rect(aes(xmin=px-xd,ymin=py-yd,
                                xmax=px+xd,ymax=py+yd,fill=pred)) +
        geom_point(aes(x=px3,y=py3), data=tiny%>%sample_n(1000), alpha=.4, color="white")
}

tiny$pred_raw <- predict(model_raw, tiny %>% select(pickup_latitude, pickup_longitude))
test$pred_raw <- predict(model_raw, test %>% select(pickup_latitude, pickup_longitude))
tiny$pred_xy  <- predict(model_xy , tiny %>% select(px, py))
test$pred_xy  <- predict(model_xy , test %>% select(px, py))
tiny$pred_xy3 <- predict(model_xy3, tiny %>% select(px3, py3))
tiny$pred_xy4 <- predict(model_xy4, tiny %>% select(px3, py4))
tiny$pred_xy44<- predict(model_xy44,tiny %>% select(px4, py4))
tiny$pred_xy_4<- predict(model_xy_4,tiny %>% select(px , py4))
tiny$pred_xy4_<- predict(model_xy4_,tiny %>% select(px4, py ))
tiny$pred_xyd <- predict(model_xyd, tiny %>% select(px, py, trip_distance))
tiny$pred_xyh <- predict(model_xyh, tiny %>% select(px, py, h))
rmse <- function(pred, act) round(mean((pred - act)^2),3)
par(mfrow=c(2,1))
rmse(tiny$pred_raw, tiny$tip_amount); rmse(test$pred_raw, tiny$tip_amount)
rmse(tiny$pred_xy , tiny$tip_amount); rmse(test$pred_xy , tiny$tip_amount)
rmse(tiny$pred_xy3, tiny$tip_amount)
rmse(tiny$pred_xy4, tiny$tip_amount)
rmse(tiny$pred_xy44,tiny$tip_amount)
rmse(tiny$pred_xy_4,tiny$tip_amount)
rmse(tiny$pred_xy4_,tiny$tip_amount)
rmse(tiny$pred_xyd, tiny$tip_amount) # of course, smallest
rmse(tiny$pred_xyh, tiny$tip_amount)
#plot(tiny$pred_raw, tiny$tip_amount);
#plot(tiny$pred_xy, tiny$tip_amount);
#plot(tiny$pred_xyh, tiny$tip_amount);
#plot(tiny$pred_xydist, tiny$tip_amount);

plot(sort(abs(test$tip_amount - test$pred_xy)), pch=18)
points(sort(abs(test$tip_amount - test$pred_raw)), pch=3, col="blue")
table(tiny$pred_xy != tiny$tip_amount)
table(tiny$pred_raw != tiny$tip_amount)

#roundXlim <- c(-83.79,-83.745)
#roundYlim <- c(-10.889, -10.73)
png("EDA/univWeek-raw-xy.png", width=640, height=1280)
    plot(tiny$px , tiny$py , pch=18, xlab="", ylab="")
dev.off()

png("EDA/univWeek-rounded-xy.png", width=640, height=1280)
    plot(tiny$px3, tiny$py3, pch=18, xlab="", ylab="")
dev.off()

plot(tiny$px4, tiny$py4, pch=18, xlab="", ylab="")

paste0("Decision Tree rpart( tip ~ x + y ) RMSE = ",rmse(test$pred_raw, test$tip_amount))
paste0("Decision Tree rpart( tip ~ x + y ) RMSE = ",rmse(test$pred_xy, test$tip_amount)) # 6% reduce with the same model

#model_xy$frame %>% filter(var == "<leaf>" & n == 1)
#model_raw$frame %>% filter(var == "<leaf>" & n == 1)

if( FALSE ) {
    # Do Non-manhattan areas get benefits by rotation?
valid <- univWeek %>% filter(! inMt ) %>%
    mutate( py = cos(theta) * pickup_latitude  - sin(theta) * pickup_longitude,
            px = sin(theta) * pickup_latitude  + cos(theta) * pickup_longitude,
            dy = cos(theta) * dropoff_latitude - sin(theta) * dropoff_longitude,
            dx = sin(theta) * dropoff_latitude + cos(theta) * dropoff_longitude,
            h = hour(pickup_datetime) ) %>% 
    filter( px < -83.7 & -12 < py & py < -10 ) %>% # remove JFK
    filter( payment_type == 1 ) %>%
    sample_n(100000)

plot(valid$pickup_longitude, valid$pickup_latitude, pch=18, xlab="", ylab="", cex=.6, 
     main="Weekly Pickup Location in Non-Manhattan \n(n=100,000)")

plot(valid$px, valid$py, pch=18, xlab="", ylab="", cex=.6, 
     main="Weekly Pickup Location in Non-Manhattan \n(n=100,000)")

valid_raw <- rpart( tip_amount ~ ., data=valid %>% select(tip_amount, pickup_longitude, pickup_latitude), control=ctl)
valid_xy  <- rpart( tip_amount ~ ., data=valid %>% select(tip_amount,              px ,             py ), control=ctl)

valid$pred_raw <- predict(valid_raw, valid %>% select(pickup_latitude, pickup_longitude))
valid$pred_xy  <- predict(valid_xy , valid %>% select(px, py))
rmse(valid$pred_raw, valid$tip_amount)
rmse(valid$pred_xy , valid$tip_amount)

plot(valid$px, valid$py, pch=18, xlab="", ylab="", cex=.6, 
     main="Weekly Pickup Location in Non-Manhattan \n(n=100,000)")

}

library(ranger)

rangered <- ranger(data = tiny[1:1000, c('px', 'py', 'tip_amount')], num.trees = 100, write.forest = FALSE, save.memory = FALSE, dependent.variable.name = "tip_amount")

rangered <- ranger(data = tiny[1:1000, c('pickup_longitude', 'pickup_latitude', 'tip_amount')], num.trees = 1000, write.forest = FALSE, save.memory = FALSE, dependent.variable.name = "tip_amount")

library(randomForest)

rf <- randomForest(tiny[1:1000, c('pickup_longitude', 'pickup_latitude', 'trip_distance')], tiny[1:1000, c('tip_amount')], ntree = 1000)

tr <- tiny[   1:1000, c('pickup_longitude', 'pickup_latitude')]
te <- tiny[1001:2000, c('pickup_longitude', 'pickup_latitude')]
y <- tiny$tip_amount[1:1000]
reged <- FNN::knn.reg(tr,te,y,k=5, algorithm=c("kd_tree", "cover_tree", "brute")[3])
mean((reged$pred - y)^2)

tr <- tiny[   1:1000, c('px', 'py')]
te <- tiny[1001:2000, c('px', 'py')]
y <- tiny$tip_amount[1:1000]
reged <- FNN::knn.reg(tr,te,y,k=5, algorithm=c("kd_tree", "cover_tree", "brute")[3])
mean((reged$pred - y)^2)

library(kknn)

k2 <- train.kknn( tip_amount ~ px + py, data = tiny[1:1000, ], distance = 2 )
k1 <- train.kknn( tip_amount ~ pickup_longitude + pickup_latitude, data = tiny[1:1000, ], distance=1 )

scaled <- tiny %>% mutate( px = (px-mean(px)) / sd(px), py = (py-mean(py))/sd(py), h = (h-mean(h))/sd(h) )

tr <- scaled[1:50000,]
te <- scaled[50001:51000,]

kknned <- kknn( tip_amount ~ pickup_longitude + pickup_latitude, train=tr, test=te, distance = 1, k=11)
mean((te$tip_amount - kknned$fitted.values)^2)
kknned <- kknn( tip_amount ~ pickup_longitude + pickup_latitude, train=tr, test=te, distance = 2, k=11)
mean((te$tip_amount - kknned$fitted.values)^2)

kknned <- kknn( tip_amount ~ px + py, train=tr, test=te, distance = 1, k=11)
mean((te$tip_amount - kknned$fitted.values)^2)
kknned <- kknn( tip_amount ~ px + py, train=tr, test=te, distance = 2, k=11)
mean((te$tip_amount - kknned$fitted.values)^2)

kknned <- kknn( tip_amount ~ px + py + h, train=tr, test=te, distance = 1, k=11)
mean((te$tip_amount - kknned$fitted.values)^2)

#library(plotly)
#plot_ly(tiny, x = ~ x, y = ~ y )
