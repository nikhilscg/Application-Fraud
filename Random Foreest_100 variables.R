library(randomForest)
library(dplyr)
###let us use random forest for feature selection

##read the data
model_data = read.csv("10foldCrossValidated_Lasso_Features.csv")[,2:24]

set.seed(26)

sample <- sample.int(n = nrow(model_data), size = floor(.70*nrow(model_data)), replace = F)
training <- model_data[sample, ]
test  <- model_data[-sample, ]
# training$fraud=as.factor(training$fraud)
# test$fraud=as.factor(test$fraud)
train_1=data.frame()
test_1=data.frame()


set.seed(1)
rf = randomForest(fraud~.,data = training[,3:23])
print(rf)
plot(rf)
##tune mtry
t = tuneRF(training[,4:23],training$fraud,
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 300,
       trace = TRUE,
       improve=0.0005)

##as the tuning gives mtry =3 so we use that in out model

library(caret)
pred.train = predict(rf,training[,3:23])
pred.test = predict(rf,test[,3:23])

train_1=cbind(training,pred.train)
test_1=cbind(test,pred.test)
head(pred.train)
generate_report = function(training_1){
  fdr=data.frame()
  total_records = nrow(training_1)
  num_records_bin = floor(1/100*total_records)
  total_bad_pop = sum(training_1$fraud)
  total_good_pop = total_records-total_bad_pop
  for (i in c(1:100)){
    
    cumulative_records = floor(i/100*total_records)
    num_good_bin = nrow(training_1[(floor((i-1)/100*total_records)+1):floor(i/100*total_records),])-sum(training_1[(floor((i-1)/100*total_records)+1):floor(i/100*total_records),]$fraud)
    num_bad_bin = sum(training_1[(floor((i-1)/100*total_records)+1):floor(i/100*total_records),]$fraud)
    cumulative_num_good = floor(i/100*total_records)-sum(training_1[1:floor(i/100*total_records),]$fraud)
    cumulative_num_bad = sum(training_1[1:floor(i/100*total_records),]$fraud)
    
    fdr[i,"population bin %"]=paste(i,"%",sep="")
    fdr[i,"total number of records"]=num_records_bin
    fdr[i,"# Good"] = num_good_bin
    fdr[i,"# Bad"] = num_bad_bin
    fdr[i,"% Good"] = 100* num_good_bin/(num_good_bin+num_bad_bin)
    fdr[i,"% Bad"]=100*(num_bad_bin/(num_good_bin+num_bad_bin))
    
    fdr[i,"cumulative good"]=cumulative_num_good
    fdr[i,"cumulative bad"]=cumulative_num_bad
    fdr[i,"% Good out of total Goods in population"]= 100*cumulative_num_good/total_good_pop
    fdr[i,"% Bad (FDR)"]=100*cumulative_num_bad/total_bad_pop
    
    
    
  }
  return(fdr)
}
test_1$pred.test=as.numeric(test_1$pred.test)
test_1%>%
  arrange(-pred.test)-> test_1
train_1$fraud=as.numeric(train_1$fraud)

train_1$pred.train=as.numeric(train_1$pred.train)
test_1$fraud=as.numeric(test_1$fraud)

train_1%>%
  arrange(-pred.train)-> train_1
fdr_rf_training = generate_report(train_1)
fdr_rf_test = generate_report(test_1)
##oot sample
outoftime = read.csv("outoftimesample.csv")[,2:205]
myvar = names(outoftime)[names(outoftime) %in% names(training)]
oot_reduced = outoftime[myvar]

predict.oot = predict(rf,oot_reduced[,c(2,4:23)])
oot_reduced=cbind(oot_reduced,predict.oot)
oot_reduced%>%
  arrange(-predict.oot) -> oot_reduced

fdr_rf_oot = generate_report(oot_reduced)
##12.13 % fter tuning th RF also
##what other things can be done? should i use PCA?