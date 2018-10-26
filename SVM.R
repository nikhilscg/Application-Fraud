library(dplyr)
library(e1071)

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
