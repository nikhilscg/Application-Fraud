library(dplyr)
library(lubridate)
all_variables1 = read.csv("all_variables_228.csv")[,2:229]
original_data = read.csv("applications.csv")
# all_variables%>%
#   filter(ssn==737610282) -> check1
# all_variables%>%
#   filter(homephone==9105580920)-> check3
all_variables=all_variables1

all_variables=cbind(all_variables,original_data$homephone,original_data$ssn)
colnames(all_variables)[229:230]=c("homephone","ssn")

##to set frivolous values to 0



for (i in 1:nrow(all_variables)){
  if(all_variables[i,"ssn"]==737610282){
    all_variables[i,"ssn_count_3"]=0
    all_variables[i,"ssn_count_10"]=0
    all_variables[i,"ssn_count_20"]=0
    all_variables[i,"ssn_count_30"]=0
    all_variables[i,"fullnamessn_count_30"]=0
    all_variables[i,"fullnamessn_count_3"]=0
    all_variables[i,"fullnamessn_count_20"]=0
    all_variables[i,"fullnamessn_count_10"]=0
    
    all_variables[i,"ssn_zip5_distinct_3"]=0
    all_variables[i,"ssn_homephone_distinct_3"]=0
    all_variables[i,"ssn_address_distinct_3"]=0
    all_variables[i,"ssn_fullnameDOB_distinct_3"]=0
    all_variables[i,"ssn_fullnamePhone_distinct_3"]=0
  
    all_variables[i,"ssn_fullnamezip5_distinct_3"]=0
    all_variables[i,"ssn_fullnamessn_distinct_3"]=0
    all_variables[i,"ssn_zip5_distinct_10"]=0
    all_variables[i,"ssn_homephone_distinct_10"]=0
    all_variables[i,"ssn_address_distinct_10"]=0
    all_variables[i,"ssn_fullnameDOB_distinct_10"]=0
    all_variables[i,"ssn_fullnamePhone_distinct_10"]=0
    
    all_variables[i,"ssn_fullnamezip5_distinct_10"]=0
    all_variables[i,"ssn_fullnamessn_distinct_10"]=0
    all_variables[i,"ssn_zip5_distinct_20"]=0
    all_variables[i,"ssn_homephone_distinct_20"]=0
    all_variables[i,"ssn_address_distinct_20"]=0
    all_variables[i,"ssn_fullnameDOB_distinct_20"]=0
    all_variables[i,"ssn_fullnamePhone_distinct_20"]=0
    
    all_variables[i,"ssn_fullnamezip5_distinct_20"]=0
    all_variables[i,"ssn_fullnamessn_distinct_20"]=0
    all_variables[i,"ssn_zip5_distinct_30"]=0
    all_variables[i,"ssn_homephone_distinct_30"]=0
    all_variables[i,"ssn_address_distinct_30"]=0
    all_variables[i,"ssn_fullnameDOB_distinct_30"]=0
    all_variables[i,"ssn_fullnamePhone_distinct_30"]=0
    
    all_variables[i,"ssn_fullnamezip5_distinct_30"]=0
    all_variables[i,"ssn_fullnamessn_distinct_30"]=0
    
    
    
    }
}
##now for frivolow homephone
for (i in 1:nrow(all_variables)){
  if(all_variables[i,"homephone"]==9105580920){
    all_variables[i,"homephone_count_3"]=0
    all_variables[i,"homephone_count_10"]=0
    all_variables[i,"homephone_count_20"]=0
    all_variables[i,"homephone_count_30"]=0
    all_variables[i,"fullnamePhone_count_3"]=0
    all_variables[i,"fullnamePhone_count_10"]=0
    all_variables[i,"fullnamePhone_count_20"]=0
    all_variables[i,"fullnamePhone_count_30"]=0
    all_variables[i,"phone_zip5_distinct_3"]=0
    all_variables[i,"phone_ssn_distinct_3"]=0
    all_variables[i,"phone_address_distinct_3"]=0
    all_variables[i,"phone_fullnameDOB_distinct_3"]=0
    all_variables[i,"phone_fullnamePhone_distinct_3"]=0
    
    all_variables[i,"phone_fullnamezip5_distinct_3"]=0
    all_variables[i,"phone_fullnamessn_distinct_3"]=0
    all_variables[i,"phone_zip5_distinct_10"]=0
    all_variables[i,"phone_ssn_distinct_10"]=0
    all_variables[i,"phone_address_distinct_10"]=0
    all_variables[i,"phone_fullnameDOB_distinct_10"]=0
    all_variables[i,"phone_fullnamePhone_distinct_10"]=0
    
    all_variables[i,"phone_fullnamezip5_distinct_10"]=0
    all_variables[i,"phone_fullnamessn_distinct_10"]=0
    all_variables[i,"phone_zip5_distinct_20"]=0
    all_variables[i,"phone_ssn_distinct_20"]=0
    all_variables[i,"phone_address_distinct_20"]=0
    all_variables[i,"phone_fullnameDOB_distinct_20"]=0
    all_variables[i,"phone_fullnamePhone_distinct_20"]=0
    
    all_variables[i,"phone_fullnamezip5_distinct_20"]=0
    all_variables[i,"phone_fullnamessn_distinct_20"]=0
    all_variables[i,"phone_zip5_distinct_30"]=0
    all_variables[i,"phone_ssn_distinct_30"]=0
    all_variables[i,"phone_address_distinct_30"]=0
    all_variables[i,"phone_fullnameDOB_distinct_30"]=0
    all_variables[i,"phone_fullnamePhone_distinct_30"]=0
    
    all_variables[i,"phone_fullnamezip5_distinct_30"]=0
    all_variables[i,"phone_fullnamessn_distinct_30"]=0
    
    
    
    
  }
}
##remove firstname and lastname concatenated fields
all_variables%>%
  select(-contains("firstname"))%>%
  select(-contains("lastname"))%>%
  select(-ssn,-homephone)->all_variables
##ks test

##read in the new variables
new_variables=read.csv("new_variables_correct_frivolous.csv")[,2:10]
all_variables=cbind(all_variables,new_variables[,6:9])

ks=all_variables
ks$date=as.character(ks$date)
ks$date=as.Date(ks$date)
outoftime = ks%>%
  filter(date>=mdy("11-01-2016"))
model_data1 = ks%>%
  filter(date<mdy("11-01-2016"))

write.csv(outoftime,"final_out_of_time.csv")

?ks.test
length(model_data1)
ks.distance = vector()
test$statistic

for (i in 4:length(model_data1)){
  x = data.frame(model_data1%>%
    filter(fraud == 0))[,i]
  y =  data.frame(model_data1%>%
                    filter(fraud == 1))[,i] 
  ks.distance[i-3] = ks.test(x,y)$statistic
}
#colnames(model_data1)[4:206]
names(ks.distance)=colnames(model_data1)[4:length(model_data1)]
names(ks.distance)
ks.distance
ks_stat = rbind(model_data1[,4:length(model_data1)],ks.distance)
reordered=ks_stat[,order(-ks_stat[nrow(ks_stat),])]

##also take the last 100 predictors

reordered = reordered[1:nrow(reordered)-1,]
reordered = cbind(model_data1$record,model_data1$date,model_data1$fraud,reordered)
##select the last 100 
last_100 = reordered[,c(1:3,104:length(reordered))]
##select the top 100 predictors
reordered = reordered[,1:103]
colnames(reordered)[1:3]=c("record","date","fraud")
colnames(last_100)[1:3]=c("record","date","fraud")

###save the dataset for input to forward selection
write.csv(reordered,"After_KS.csv")
write.csv(model_data1,"model_data_beforeKS.csv")
write.csv(outoftime,"outoftimesample.csv")
write.csv(last_100,"last_100_predictors_after KS.csv")
