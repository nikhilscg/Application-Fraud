library(dplyr)
##read in the data
feature_selection_data = read.csv("After_KS.csv")

reordered = feature_selection_data[,2:104]

## Run forward selection using logistic regression
backward.fit = lm(fraud~.,data = reordered[,3:103])
#summary(backward.fit)
formula(backward.fit)
###backward fit
#step(backward.fit,direction="backward")


##forward fit
fitstart = lm(fraud~1,data = reordered[,3:103])
summary(fitstart)
#step(fitstart,direction="forward",scope=formula(backward.fit))

#plot(ecdf(outoftime$ssn_count_10))

# ##using logistic regression
# back = glm(fraud~.,data = reordered[,2:20],family="binomial")
# forwardstart = glm(fraud~1,data=reordered[,2:20],family="binomial")
# summary(forwardstart)
# step(forwardstart,direction="forward",scope = formula(back))
# step(back,direction="backward")
# ##both directions
# step(forwardstart,direction="both",scope=formula(back))

forward_selection_model = step(fitstart,direction="forward",scope=formula(backward.fit))
##add phone_ssn_distinct_3 as sum of squares higher than others
selected_features=forward_selection_model$coefficients
selected_features
##subset the reordered data into the model data from which training and testing would be made
myvar = names(reordered)[names(reordered) %in% c(names(selected_features),"phone_ssn_distinct_3")]
model_data=reordered[myvar]
model_data=cbind(reordered$record,reordered$date,reordered$fraud,model_data)
colnames(model_data)[1:3]=c("record","date","fraud")


set.seed(26)

sample <- sample.int(n = nrow(model_data), size = floor(.70*nrow(model_data)), replace = F)
training <- model_data[sample, ]
test  <- model_data[-sample, ]

##linear regression model
lm.fit =lm(fraud~.,data=training[,3:23])
##summary of model
summary(lm.fit)
#predict the value of fraud measure
lm.predict = predict(lm.fit,training[3:23])
lm.predict.test = predict(lm.fit,test[3:23])
##add this to the model_data 
train_1=data.frame()
test_1=data.frame()

train_1=cbind(training,lm.predict)
test_1 = cbind(test,lm.predict.test)
test_1%>%
  arrange(-lm.predict.test)-> test_1

train_1%>%
  arrange(-lm.predict)-> train_1

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
fdr_linear_training = generate_report(train_1)

##write out the results file

write.csv(fdr_linear_training,"linear_train_results_table.csv")
#now check for test datasets
fdr_linear_test = generate_report(test_1)
##write out the report
write.csv(fdr_linear_test,"linear_test_results_table.csv")
##now check for the out of time sample
outoftime = read.csv("outoftimesample.csv")[,2:205]
myvar = names(outoftime)[names(outoftime) %in% names(training)]
oot_reduced = outoftime[myvar]

predict.oot = predict(lm.fit,oot_reduced[,c(2,4:23)])
oot_reduced=cbind(oot_reduced,predict.oot)
oot_reduced%>%
  arrange(-predict.oot) -> oot_reduced

fdr_linear_oot = generate_report(oot_reduced)
##write out the report
write.csv(fdr_linear_oot,"linear_oot_results_table")


##Boosted Trees


##SVM

##Random Forests


