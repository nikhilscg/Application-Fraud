
library(dplyr)
##read int he feature selection dataset for the linear regression with 20 predictors
model_data=read.csv("Forward_Selection_Features.csv")[,2:24]


set.seed(26)

sample <- sample.int(n = nrow(model_data), size = floor(.70*nrow(model_data)), replace = F)
training <- model_data[sample, ]
test  <- model_data[-sample, ]

##logistic regression model
log.fit =glm(fraud~.,data=training[,3:23])
##summary of model
summary(log.fit)
#predict the value of fraud measure
log.predict = predict(log.fit,training[3:23],type="response")
log.predict.test = predict(log.fit,test[3:23],type="response")
##add this to the model_data 
train_1=data.frame()
test_1=data.frame()

train_1=cbind(training,log.predict)
test_1 = cbind(test,log.predict.test)
test_1%>%
  arrange(-log.predict.test)-> test_1

train_1%>%
  arrange(-log.predict)-> train_1

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
fdr_logistic_training = generate_report(train_1)

##write out the results file

write.csv(fdr_logistic_training,"logistic_train_results_table_20 features.csv")
#now check for test datasets
fdr_logistic_test = generate_report(test_1)
##write out the report
write.csv(fdr_logistic_test,"logistic_test_results_table_20 features.csv")
##now check for the out of time sample
outoftime = read.csv("outoftimesample.csv")[,2:205]
myvar = names(outoftime)[names(outoftime) %in% names(training)]
oot_reduced = outoftime[myvar]

predict.oot = predict(log.fit,oot_reduced[,c(2,4:23)],type="response")
oot_reduced=cbind(oot_reduced,predict.oot)
oot_reduced%>%
  arrange(-predict.oot) -> oot_reduced

fdr_logistic_oot = generate_report(oot_reduced)
##write out the report
write.csv(fdr_logistic_oot,"logistic_oot_results_table_20 features.csv")


##Boosted Trees

##neural Nets

##SVM

##Random Forests


