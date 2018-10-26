library(dplyr)
library(glmnet)
library(neuralnet)

feature_selection_data = read.csv("After_KS.csv")

reordered = feature_selection_data[,2:104]

##cross validation to find optimum lambda value
CV = cv.glmnet(x=as.matrix(reordered[,4:103]),y=reordered[,3],alpha=1,nlambda=100,nfolds=10)

#alphga =1 means we are doing lasso, 0 means ridge

plot(CV)

##fit the model
fit = glmnet(x=as.matrix(reordered[,4:103]),y=reordered[,3],alpha=1,lambda = CV$lambda.min)
fit$beta
lasso_coeff = fit$beta[,1]

lasso_coeff
selected_features=names(lasso_coeff)[lasso_coeff!=0]
selected_features
##subset the reordered data into the model data from which training and testing would be made
myvar = names(reordered)[names(reordered) %in% selected_features]
model_data=reordered[myvar]
model_data=cbind(reordered$record,reordered$date,reordered$fraud,model_data)
colnames(model_data)[1:3]=c("record","date","fraud")
##write out the moidel_data
write.csv(model_data,"10foldCrossValidated_Lasso_Features.csv")

#############################################################
##bagging and boosting
# library(RCurl)
# binData <- getBinaryURL("https://archive.ics.uci.edu/ml/machine-learning-databases/00296/dataset_diabetes.zip",ssl.verifypeer=FALSE)
# 
# conObj <- file("dataset_diabetes.zip", open = "wb")
# writeBin(binData, conObj)
# # don't forget to close it
# close(conObj)
# 
# # open diabetes file
# files <- unzip("dataset_diabetes.zip")
# diabetes <- read.csv(files[1], stringsAsFactors = FALSE)
# 
# # drop useless variables
# diabetes <- subset(diabetes,select=-c(encounter_id, patient_nbr))
# 
# # transform all "?" to 0s
# diabetes[diabetes == "?"] <- NA
# 
# # remove zero variance - ty James http://stackoverflow.com/questions/8805298/quickly-remove-zero-variance-variables-from-a-data-frame
# diabetes <- diabetes[sapply(diabetes, function(x) length(levels(factor(x,exclude=NULL)))>1)]
# 
# # prep outcome variable to those readmitted under 30 days
# diabetes$readmitted <- ifelse(diabetes$readmitted == "<30",1,0)
# 
# # generalize outcome name
# outcomeName <- 'readmitted'
# 
# # drop large factors
# diabetes <- subset(diabetes, select=-c(diag_1, diag_2, diag_3))
# 
# 
# # binarize data
# charcolumns <- names(diabetes[sapply(diabetes, is.character)])
# for (colname in charcolumns) {
#   print(paste(colname,length(unique(diabetes[,colname]))))
#   for (newcol in unique(diabetes[,colname])) {
#     if (!is.na(newcol))
#       diabetes[,paste0(colname,"_",newcol)] <- ifelse(diabetes[,colname]==newcol,1,0)
#   }
#   diabetes <- diabetes[,setdiff(names(diabetes),colname)]
# }
# 
# # remove all punctuation characters in column names after binarization that could trip R
# colnames(diabetes) <- gsub(x =colnames(diabetes), pattern="[[:punct:]]", replacement = "_" )
# 
# # check for zero variance - ty James http://stackoverflow.com/questions/8805298/quickly-remove-zero-variance-variables-from-a-data-frame
# diabetes <- diabetes[sapply(diabetes, function(x) length(levels(factor(x,exclude=NULL)))>1)]
# 
# # transform all NAs into 0
# diabetes[is.na(diabetes)] <- 0 
# 
# # split data set into training and testing
# set.seed(1234)
# split <- sample(nrow(diabetes), floor(0.5*nrow(diabetes)))
# traindf <- diabetes[split,]
# testdf <-  diabetes[-split,]
# 
# predictorNames <- setdiff(names(traindf), outcomeName)
# fit <- lm(readmitted ~ ., data = traindf)
# preds <- predict(fit, testdf[,predictorNames], se.fit = TRUE)
# 
# library(pROC)
# print(auc(testdf[,outcomeName], preds$fit))
# 
# ###use bagging and boosting to fit the neural net

##read the data
model_data=read.csv("10foldCrossValidated_Lasso_Features.csv")[,2:24]

##run neural nets
##min-max normalization

maxValue = apply(model_data[,4:23],2,max)
minValue = apply(model_data[,4:23],2,min)
df = as.data.frame(scale(model_data[,4:23],center = minValue,scale=maxValue))
model_data[,4:23]=df

set.seed(26)

sample <- sample.int(n = nrow(model_data), size = floor(.70*nrow(model_data)), replace = F)
training <- model_data[sample, ]
test  <- model_data[-sample, ]

myvars = colnames(training)
predictorvars = myvars[!myvars %in% c("record","fraud","date")]
predictorvars=paste(predictorvars,collapse = "+")
form = as.formula(paste("fraud~",predictorvars,collapse="+"))
?neuralnet
neuralModel = neuralnet(formula=form,
                        hidden=10,
                        err.fct = "ce",
                        linear.output = FALSE,
                        data = training[,3:23]
                        )
plot(neuralModel)




##predict for the test data
predictions = compute(neuralModel,testdf[,1:13])
str(predictions)
predictions=predictions$net.result*(max(testdf$medv)-min(testdf$medv))+min(testdf$medv)
actualvalues = testdf$medv*(max(testdf$medv)-min(testdf$medv))+min(testdf$medv)
MSE = sum((predictions-actualvalues)^2)/nrow(testdf)
MSE
