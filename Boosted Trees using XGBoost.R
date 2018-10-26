library(xgboost)
library(dplyr)
library(magrittr)
library(Matrix)

##read the data
model_data = read.csv("Linear_Forward_Selection_Features.csv")[,2:23]
cov_matrix = cov(as.matrix(model_data[,4:23]))

set.seed(26)
# ind = sample(2,nrow(model_data),replace=T,prob=c(0.7,0.3))
# training = model_data[ind==1,]
# test = model_data[ind==2,]

sample <- sample.int(n = nrow(model_data), size = floor(.70*nrow(model_data)), replace = F)
training <- model_data[sample, ]
test  <- model_data[-sample, ]
# training$fraud=as.factor(training$fraud)
# test$fraud=as.factor(test$fraud)
train_1=data.frame()
test_1=data.frame()
##create matrix 

for (i in 3:length(training)){
  training[,i]=as.numeric(training[,i])
}
for (i in 3:length(test)){
  test[,i]=as.numeric(test[,i])
}

train_matrix = xgb.DMatrix(data = as.matrix(training[,4:length(training)]),label=training$fraud)
test_matrix = xgb.DMatrix(data = as.matrix(test[,4:length(test)]),label = test$fraud)

# parameters
nc = length(unique(training$fraud))
nc

xgb_params = list("objective"="reg:linear",
                  "eta"=0.1,
                  "booster"="gblinear",
                  "gamma"=0.01,
                  "maxdepth"=7,
                  "eval.metric"="rmse")
                  
                  
                  
watchlist = list(train=train_matrix, test = test_matrix)
##xtreme gradient boosting model
bst_model = xgb.train(data = train_matrix,params = xgb_params,
                      nrounds = 500)

##plot the data
e = data.frame(bst_model$evaluation_log)

plot(e$iter,e$train_mlogloss,col="blue")  
lines(e$iter,e$test_mlogloss,col="red")
min(e$test_mlogloss)

##feature importance
imp = xgb.importance(colnames(train_matrix),model=bst_model)

print(imp)
xgb.plot.importance(imp)
dim(test_matrix)
##predict on test data
pred.test = predict(bst_model,newdata = test_matrix)
pred.train=predict(bst_model,newdata=train_matrix)
head(pred.test)
test_1=cbind(test,pred.test)
test_1=test_1%>%
  arrange(-pred.test)

train_1=cbind(training,pred.train)
train_1=train_1%>%
  arrange(-pred.train)
# pred.test = matrix(pred.test,nrow=nc,ncol=length(pred.test)/nc)%>%
#   t()%>%
#   data.frame()%>%
#   mutate(label = test$fraud,max_prob = max.col(.,"last")-1)

head(pred.test)


##as the tuning gives mtry =3 so we use that in out model

# library(caret)
# pred.train = predict(rf,training[,3:23])
# pred.test = predict(rf,test[,3:23])
# 
# train_1=cbind(training,pred.train)
# test_1=cbind(test,pred.test)
# head(pred.train)
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
fdr_boostedtrees_training = generate_report(train_1)
fdr_boostedtrees_test = generate_report(test_1)
##oot sample
oot_reduced=data.frame()
outoftime = read.csv("final_out_of_time.csv")[,2:209]
ordernames = names(training)
oot_reduced=outoftime[,ordernames]
for (i in 4:length(oot_reduced)){
  oot_reduced[,i]=as.numeric(oot_reduced[,i])
}
oot_matrix = xgb.DMatrix(data=as.matrix(oot_reduced[,4:length(oot_reduced)]),label=oot_reduced$fraud)

?xgb.train

predict.oot = predict(bst_model,oot_matrix)


oot_reduced=cbind(oot_reduced,predict.oot)
oot_reduced%>%
  arrange(-predict.oot) -> oot_reduced

fdr_boostedtrees_oot = generate_report(oot_reduced)
##12.13 % fter tuning th RF also
##what other things can be done? should i use PCA?