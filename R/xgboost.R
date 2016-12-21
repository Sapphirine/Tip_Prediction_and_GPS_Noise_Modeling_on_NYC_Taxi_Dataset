source('R/set_univWeek.R', echo=TRUE)
mini <- huge[1:100000, ]
test <- huge[  100001:200000, ]
tiny <- huge[1:200000, ] %>% mutate( isCons = as.numeric(isCons))
nano <- tiny[1:500,]


library(xgboost)
library(caret)
library(dummies)
featurev <- c('cab_type_id', 'vendor_id', 'store_and_fwd_flag', 'rate_code_id', 'pickup_longitude', 'pickup_latitude', 'h', 'wday')
featurev <- c('cab_type_id', 'vendor_id', 'store_and_fwd_flag', 'rate_code_id', 'px3', 'py3', 'h', 'wday')

#as.data.frame(dummyVars( ~., data = mini[, featurev]))
dummied <- dummy.data.frame(tiny[, featurev], sep=".", dummy.classes = c("factor","ordered", "character"))

dtr <- xgb.DMatrix(as(as.matrix(dummied), 'sparseMatrix'), label=as(tiny$isCons,'sparseMatrix'))
set.seed(12345)
#xgb.train(params = list(eta=.3, max_depth=3, objective='binary:logistic'), data = dtr, nrounds = 100, metrics=list("error"))
xgb.cv(params = list(eta=.3, max_depth=4, objective='binary:logistic', alpha=2), data = dtr, nrounds = 100, nfold = 2, metrics=list("error"), print_every_n = 10)

library(ranger)
ed2 <- ranger(data = bind_cols(dummied, data.frame( isCons = tiny$isCons)), write.forest = FALSE, importance = "permutation", num.trees = 1000, dependent.variable.name = "isCons", classification = TRUE)


dummied <- dummy.data.frame(tiny[, featurev], sep=".", dummy.classes = c("factor","ordered", "character"))


modeled <- model.matrix( ~ cab_type_id + vendor_id + store_and_fwd_flag + rate_code_id + px2 + py2 + h2:wday + 0,tiny %>% mutate( h2 = as.factor(h2)) )

ed3 <- ranger(data = bind_cols(data.frame(modeled), data.frame( isCons = tiny$isCons)), write.forest = FALSE, importance = "permutation", num.trees = 500, dependent.variable.name = "isCons", classification = TRUE)

modeled <- model.matrix( ~ cab_type_id + vendor_id + store_and_fwd_flag + rate_code_id + pickup_longitude + pickup_latitude + h2:isWeekday + 0,tiny %>% mutate( h2 = as.factor(h2)) )

ed5 <- ranger(data = bind_cols(data.frame(modeled), data.frame( isCons = tiny$isCons)), write.forest = FALSE, num.trees = 250, dependent.variable.name = "isCons", classification = TRUE)

ed6 <- ranger(data = bind_cols(data.frame(model.matrix( ~ cab_type_id + vendor_id + store_and_fwd_flag + rate_code_id + pickup_longitude + pickup_latitude + h2:isWeekday + consRate + 0, tiny %>% mutate( h2 = as.factor(h2)) )), data.frame( isCons = tiny$isCons)), write.forest = FALSE, num.trees = 250, dependent.variable.name = "isCons", classification = TRUE) #  consRate - 2% improve !

ed7 <- ranger(data = bind_cols(data.frame(model.matrix( ~ cab_type_id + vendor_id + store_and_fwd_flag + rate_code_id + pickup_longitude + pickup_latitude + h2:isWeekday + consRate + pid + passenger_count + 0, tiny %>% mutate( h2 = as.factor(h2)) )), data.frame( isCons = tiny$isCons)), write.forest = FALSE, num.trees = 250, dependent.variable.name = "isCons", classification = TRUE) # slow and worse

ed8 <- ranger(data = bind_cols(data.frame(model.matrix( ~ cab_type_id + vendor_id + store_and_fwd_flag + rate_code_id + pickup_longitude + pickup_latitude + h2:isWeekday + consRate + rid + passenger_count + 0, tiny %>% mutate( h2 = as.factor(h2)) )), data.frame( isCons = tiny$isCons)), write.forest = FALSE, num.trees = 250, dependent.variable.name = "isCons", classification = TRUE)

edcheat <- ranger(data = bind_cols(data.frame(model.matrix( ~ cab_type_id + vendor_id + store_and_fwd_flag + rate_code_id + pickup_longitude + pickup_latitude + h2:isWeekday + consRate + rid + trip_distance + passenger_count + 0, tiny %>% mutate( h2 = as.factor(h2)) )), data.frame( isCons = tiny$isCons)), write.forest = FALSE, num.trees = 250, dependent.variable.name = "isCons", classification = TRUE) # trip distance is not so effective

edbase <- ranger(data = bind_cols(data.frame(model.matrix( ~ store_and_fwd_flag, tiny %>% mutate( h2 = as.factor(h2)) )), data.frame( isCons = tiny$isCons)), write.forest = FALSE, num.trees = 250, dependent.variable.name = "isCons", classification = TRUE) # almost nothing

ranger(data = bind_cols(data.frame(model.matrix( ~ consRate + 0, tiny %>% mutate( h2 = as.factor(h2)) )), data.frame( isCons = tiny$isCons)), write.forest = FALSE, num.trees = 250, dependent.variable.name = "isCons", classification = TRUE) # trip distance is of course strong

library(caret)
tr<- bind_cols(data.frame(model.matrix( ~ cab_type_id + vendor_id + store_and_fwd_flag + rate_code_id + inMt + h2:isWeekday  + rid + passenger_count + 0, tiny %>% mutate( h2 = as.factor(h2)) )), data.frame( isCons = tiny$isCons))
modeled <- train(isCons ~ ., data=tr, method="glm", family=binomial, trControl=trainControl(method="cv", number=5))
PredTrain = predict(modeled, newdata=tr, type="raw") 
tbled <- table(tr$isCons, PredTrain > 0.5)
(tbled[1,1] + tbled[2,2]) / sum(tbled)

#ed10 <- ranger(data = bind_cols(data.frame(model.matrix( ~ h:wday + 0,tiny %>% mutate( h = as.factor(h)) )), data.frame( isCons = as.numeric(tiny$isCons))), write.forest = FALSE, importance = "permutation", num.trees = 300, dependent.variable.name = "isCons", classification = TRUE, save.memory = FALSE) # does not work well

#save(ed10, file="data/ranger-cons-estimation.10.Rdata")

# ed11 <- ranger(data = bind_cols(data.frame(model.matrix( ~ h2:wday + 0,tiny %>% mutate( h2 = as.factor(h2)) )), data.frame( isCons = as.numeric(tiny$isCons))), write.forest = FALSE, importance = "permutation", num.trees = 200, dependent.variable.name = "isCons", classification = TRUE, save.memory = FALSE) # does not work well


#save(ed, ed2, ed3, file="data/ranger-cons-estimation.Rdata")
# diagnostics
#plot(ed3$variable.importance)