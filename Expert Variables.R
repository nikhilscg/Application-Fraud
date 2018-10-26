library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(stringi)
library(tidyr)
library(RSQLite)
library(data.table)
mydata = read.csv("applications.csv")
cleandata=mydata
options(scipen=999)
colnames(cleandata)[1]="record"
cleandata$dob=as.character(cleandata$dob)
cleandata$date= mdy(cleandata$date)
cleandata$ssn=str_pad(cleandata$ssn,9,pad = "0")

cleandata$zip5=str_pad(cleandata$zip5,5,pad = "0")
cleandata$homephone=str_pad(cleandata$homephone,10,pad = "0")
cleandata$dob = stri_reverse(cleandata$dob)
stri_sub(cleandata$dob,3,2)=91
cleandata$dob = stri_reverse(cleandata$dob)
cleandata$dob = as.factor(cleandata$dob)

dqr_data = cleandata

##add new fields
#zip3
dqr_data%>%
  mutate(zip3 = substr(zip5,1,3)) ->dqr_data
#concatenate first name and last name
dqr_data$lastname=as.character(dqr_data$lastname)
dqr_data$firstname=as.character(dqr_data$firstname)
dqr_data%>%
  mutate(fullname = paste(firstname,lastname,sep="")) -> dqr_data
##lastname and DOB
dqr_data$dob=as.character(dqr_data$dob)
dqr_data%>%
  mutate(fullnameDOB = paste(fullname,dob,sep=""),lastnameDOB = paste(lastname,dob,sep=""),firstnameDOB=paste(firstname,dob,sep=""))->dqr_data

##fullname & phone
dqr_data%>%
  mutate(fullnamePhone = paste(fullname,homephone,sep=""),firstnamePhone = paste(firstname,homephone,sep=""),lastnamePhone = paste(lastname,homephone,sep=""))-> dqr_data
##name and zip5
dqr_data%>%
  mutate(fullnamezip5 = paste(fullname,zip5,sep=""),firstnamezip5 = paste(firstname,zip5,sep=""),lastnamezip5 = paste(lastname,zip5,sep="")) -> dqr_data


##fullname and ssn
dqr_data%>%
  mutate(fullnamessn = paste(fullname,ssn,sep="")) -> dqr_data
expert = dqr_data
write.csv(expert,"expert_variables.csv")

##to get the records for month of January

expert%>%
  arrange(date) %>%
  filter(month(date)==1) -> january_data
write.csv(january_data,"january_data.csv")
###writing the code for building the first ssn expert variable
date_begin = mdy("01-01-2016")
january_data%>%
  filter(date == date_begin)
j=0
january_data%>%
  filter(date==date_begin)%>%
  nrow()

####add the variables which count number of days since last seen
##ssn,firstnameDOB,address,zip5,homephone
i=5


january_data%>%
  filter(ssn=="823721004")
samplessn[record==6603,]$date
i=6603
january_data=expert


days_ssn=vector()
days_address=vector()
days_fullnameDOB=vector()
days_phone = vector()
print(i)
for (i in 1:nrow(january_data)){
  print(i)
  temp=january_data[i,]
  samplessn=january_data[1:i-1,]%>%
    filter(ssn==temp$ssn)
  if(nrow(samplessn)>=1){
  days_ssn[i] = min(365,temp$date - samplessn[samplessn$record==max(samplessn$record),]$date)
  }
  else {
    days_ssn[i]=temp$date-date_begin
  }
  
  sampleaddress=january_data[1:i-1,]%>%
    filter(address==temp$address)
  if(nrow(sampleaddress)>=1){
    days_address[i] = min(365,temp$date - sampleaddress[sampleaddress$record==max(sampleaddress$record),]$date)
  }
  else {
    days_address[i]=temp$date-date_begin
  }
  samplehomephone=january_data[1:i-1,]%>%
    filter(homephone==temp$homephone)
  if(nrow(samplehomephone)>=1){
    days_phone[i] = min(365,temp$date - samplehomephone[samplehomephone$record==max(samplehomephone$record),]$date)
  }
  else {
    days_phone[i]=temp$date-date_begin
  }
  samplefullnameDOB = january_data[1:i-1,]%>%
    filter(fullnameDOB==temp$fullnameDOB)
  if(nrow(samplefullnameDOB)>=1){
   days_fullnameDOB[i] = min(365,temp$date - samplefullnameDOB[samplefullnameDOB$record==max(samplefullnameDOB$record),]$date)
  }
else {
  days_fullnameDOB[i]=temp$date-date_begin
}
}
new_variables = as.data.frame(cbind(expert$record,expert$ssn,expert$address,expert$homephone,expert$fullnameDOB,days_ssn,days_address,days_phone,days_fullnameDOB))
colnames(new_variables)[1:5]=c("record","ssn","address","homephone","fullnameDOB")
new_variables$homephone=as.character(new_variables$homephone)
new_variables%>%
  filter(ssn=="737610282")->nikhil
temp=new_variables
###set the frivolous values to 365
for (i in 1:nrow(new_variables)){
  if(new_variables[i,"ssn"]=="737610282"){
    new_variables[i,"days_ssn"]=365
  }
  if(new_variables[i,"homephone"]=="9105580920"){
    new_variables[i,"days_phone"]=365
  }
}
##write it out
write.csv(new_variables,"new_variables_correct_frivolous.csv")
january_data=expert
for (i in january_data$record){
  if (january_data[i,"date"]-1 == date_begin || january_data[i,"date"]-2==date_begin){
    sample1 = january_data%>%
      filter(date >= date_begin & date<=january_data[i,"date"])
    print(sample1)
  }
  

}

expert%>%
  group_by(ssn)%>%
  summarize(count=n())%>%
  arrange(-count)

###record 50000
expert%>%
  filter(record>50000)%>%
  filter(ssn=="737610282")%>%
  select(record)

print(j)
df_full = data.frame()
dfssn_3=data.frame()
dfssn_10=data.frame()
dfssn_20=data.frame()
dfssn_30=data.frame()

dfaddress_3=data.frame()
dfaddress_10=data.frame()
dfaddress_20=data.frame()
dfaddress_30=data.frame()
dfzip5_3=data.frame()
dfzip5_10=data.frame()
dfzip5_20=data.frame()
dfzip5_30=data.frame()

dfphone_3=data.frame()
dfphone_10=data.frame()
dfphone_20=data.frame()
dfphone_30=data.frame()

dffullnameDOB_3=data.frame()
dffullnameDOB_10=data.frame()
dffullnameDOB_20=data.frame()
dffullnameDOB_30=data.frame()

dffullnamessn_3=data.frame()
dffullnamessn_10=data.frame()
dffullnamessn_20=data.frame()
dffullnamessn_30=data.frame()

# df[1,"ssn_count"]=10
# df[2,"ssn_count"]=11
# df[1,"ssn_count+30"]=34

i=0
for (i in 75238:nrow(january_data)){
  sample_record = january_data%>%
    filter(record<=january_data[i,"record"])
  #print(nrow(sample_record))
  ##for 3 day window
  sample1 = sample_record%>%
    filter(date >= max(date_begin,sample_record[i,"date"]-2) & date<=sample_record[i,"date"])
  df_full[i,"ssn_count_3"] = sum(sample1$ssn == january_data[i,"ssn"])-1
  df_full[i,"address_count_3"]=sum(sample1$address==january_data[i,"address"])-1
  df_full[i,"zip5_count_3"]=sum(sample1$zip5==january_data[i,"zip5"])-1
  df_full[i,"homephone_count_3"]=sum(sample1$homephone==january_data[i,"homephone"])-1
  df_full[i,"fullnameDOB_count_3"]=sum(sample1$fullnameDOB==january_data[i,"fullnameDOB"])-1
  df_full[i,"lastnameDOB_count_3"]=sum(sample1$lastnameDOB==january_data[i,"lastnameDOB"])-1
  df_full[i,"firstnameDOB_count_3"]=sum(sample1$firstnameDOB==january_data[i,"firstnameDOB"])-1
  df_full[i,"fullnamePhone_count_3"]=sum(sample1$fullnamePhone==january_data[i,"fullnamePhone"])-1
  df_full[i,"firstnamePhone_count_3"]=sum(sample1$firstnamePhone==january_data[i,"firstnamePhone"])-1
  df_full[i,"lastnamePhone_count_3"]=sum(sample1$lastnamePhone==january_data[i,"lastnamePhone"])-1
  df_full[i,"fullnamezip5_count_3"]=sum(sample1$fullnamezip5==january_data[i,"fullnamezip5"])-1
  df_full[i,"firstnamezip5_count_3"]=sum(sample1$firstnamezip5==january_data[i,"firstnamezip5"])-1
  df_full[i,"lastnamezip5_count_3"]=sum(sample1$lastnamezip5==january_data[i,"lastnamezip5"])-1
  df_full[i,"fullnamessn_count_3"]=sum(sample1$fullnamessn==january_data[i,"fullnamessn"])-1
  


  dfssn_3=rbind(dfssn_3,sample1%>%
          filter(ssn==january_data[i,"ssn"])%>%
          summarise(
                    ssn_zip5_distinct_3=n_distinct(zip5)-1,
                    ssn_homephone_distinct_3=n_distinct(homephone)-1,
                    ssn_address_distinct_3 = n_distinct(address)-1,
                    ssn_fullnameDOB_distinct_3 = n_distinct(fullnameDOB)-1,
                    ssn_fullnamePhone_distinct_3 = n_distinct(fullnamePhone)-1,
                    ssn_fullnamezip5_distinct_3 = n_distinct(fullnamezip5)-1,
                    ssn_fullnamessn_distinct_3 = n_distinct(fullnamessn)-1
                    )
)
  dfaddress_3=rbind(dfaddress_3,sample1%>%
                  filter(address==january_data[i,"address"])%>%
                  summarise(
                    address_zip5_distinct_3=n_distinct(zip5)-1,
                    address_homephone_distinct_3=n_distinct(homephone)-1,
                    address_ssn_distinct_3 = n_distinct(ssn)-1,
                    address_fullnameDOB_distinct_3 = n_distinct(fullnameDOB)-1,
                    address_fullnamePhone_distinct_3 = n_distinct(fullnamePhone)-1,
                    address_fullnamezip5_distinct_3 = n_distinct(fullnamezip5)-1,
                    address_fullnamessn_distinct_3 = n_distinct(fullnamessn)-1
                  )
  )
  dfzip5_3=rbind(dfzip5_3,sample1%>%
                  filter(zip5==january_data[i,"zip5"])%>%
                  summarise(
                    zip5_ssn_distinct_3=n_distinct(ssn)-1,
                    zip5_homephone_distinct_3=n_distinct(homephone)-1,
                    zip5_address_distinct_3 = n_distinct(address)-1,
                    zip5_fullnameDOB_distinct_3 = n_distinct(fullnameDOB)-1,
                    zip5_fullnamePhone_distinct_3 = n_distinct(fullnamePhone)-1,
                    zip5_fullnamezip5_distinct_3 = n_distinct(fullnamezip5)-1,
                    zip5_fullnamessn_distinct_3 = n_distinct(fullnamessn)-1
                  )
  )
  dfphone_3=rbind(dfphone_3,sample1%>%
                      filter(homephone==january_data[i,"homephone"])%>%
                      summarise(
                        phone_zip5_distinct_3=n_distinct(zip5)-1,
                        phone_address_distinct_3=n_distinct(address)-1,
                        phone_ssn_distinct_3 = n_distinct(ssn)-1,
                        phone_fullnameDOB_distinct_3 = n_distinct(fullnameDOB)-1,
                        phone_fullnamePhone_distinct_3 = n_distinct(fullnamePhone)-1,
                        phone_fullnamezip5_distinct_3 = n_distinct(fullnamezip5)-1,
                        phone_fullnamessn_distinct_3 = n_distinct(fullnamessn)-1
                      )
  )
  dffullnameDOB_3=rbind(dffullnameDOB_3,sample1%>%
                    filter(fullnameDOB==january_data[i,"fullnameDOB"])%>%
                    summarise(
                      fullnameDOB_zip5_distinct_3=n_distinct(zip5)-1,
                      fullnameDOB_address_distinct_3=n_distinct(address)-1,
                      fullnameDOB_ssn_distinct_3 = n_distinct(ssn)-1,
                      fullnameDOB_phone_distinct_3 = n_distinct(homephone)-1,
                      fullnameDOB_fullnamePhone_distinct_3 = n_distinct(fullnamePhone)-1,
                      fullnameDOB_fullnamezip5_distinct_3 = n_distinct(fullnamezip5)-1,
                      fullnameDOB_fullnamessn_distinct_3 = n_distinct(fullnamessn)-1
                    )
  )
  dffullnamessn_3=rbind(dffullnamessn_3,sample1%>%
                          filter(fullnamessn==january_data[i,"fullnamessn"])%>%
                          summarise(
                            fullnamessn_zip5_distinct_3=n_distinct(zip5)-1,
                            fullnamessn_address_distinct_3=n_distinct(address)-1,
                            fullnamessn_ssn_distinct_3 = n_distinct(ssn)-1,
                            fullnamessn_phone_distinct_3 = n_distinct(homephone)-1,
                            fullnamessn_fullnamePhone_distinct_3 = n_distinct(fullnamePhone)-1,
                            fullnamessn_fullnamezip5_distinct_3 = n_distinct(fullnamezip5)-1,
                            fullnamessn_fullnameDOB_distinct_3 = n_distinct(fullnameDOB)-1
                          )
  )
  ##for 10 days
  sample1 = sample_record%>%
    filter(date >= max(date_begin,sample_record[i,"date"]-9) & date<=sample_record[i,"date"])
  df_full[i,"ssn_count_10"] = sum(sample1$ssn == january_data[i,"ssn"])-1
  df_full[i,"address_count_10"]=sum(sample1$address==january_data[i,"address"])-1
  df_full[i,"zip5_count_10"]=sum(sample1$zip5==january_data[i,"zip5"])-1
  df_full[i,"homephone_count_10"]=sum(sample1$homephone==january_data[i,"homephone"])-1
  df_full[i,"fullnameDOB_count_10"]=sum(sample1$fullnameDOB==january_data[i,"fullnameDOB"])-1
  df_full[i,"lastnameDOB_count_10"]=sum(sample1$lastnameDOB==january_data[i,"lastnameDOB"])-1
  df_full[i,"firstnameDOB_count_10"]=sum(sample1$firstnameDOB==january_data[i,"firstnameDOB"])-1
  df_full[i,"fullnamePhone_count_10"]=sum(sample1$fullnamePhone==january_data[i,"fullnamePhone"])-1
  df_full[i,"firstnamePhone_count_10"]=sum(sample1$firstnamePhone==january_data[i,"firstnamePhone"])-1
  df_full[i,"lastnamePhone_count_10"]=sum(sample1$lastnamePhone==january_data[i,"lastnamePhone"])-1
  df_full[i,"fullnamezip5_count_10"]=sum(sample1$fullnamezip5==january_data[i,"fullnamezip5"])-1
  df_full[i,"firstnamezip5_count_10"]=sum(sample1$firstnamezip5==january_data[i,"firstnamezip5"])-1
  df_full[i,"lastnamezip5_count_10"]=sum(sample1$lastnamezip5==january_data[i,"lastnamezip5"])-1
  df_full[i,"fullnamessn_count_10"]=sum(sample1$fullnamessn==january_data[i,"fullnamessn"])-1
  
  #
  dfssn_10=rbind(dfssn_10,sample1%>%
                   filter(ssn==january_data[i,"ssn"])%>%
                   summarise(
                     ssn_zip5_distinct_10=n_distinct(zip5)-1,
                     ssn_homephone_distinct_10=n_distinct(homephone)-1,
                     ssn_address_distinct_10 = n_distinct(address)-1,
                     ssn_fullnameDOB_distinct_10 = n_distinct(fullnameDOB)-1,
                     ssn_fullnamePhone_distinct_10 = n_distinct(fullnamePhone)-1,
                     ssn_fullnamezip5_distinct_10 = n_distinct(fullnamezip5)-1,
                     ssn_fullnamessn_distinct_10 = n_distinct(fullnamessn)-1
                   )
  )
  dfaddress_10=rbind(dfaddress_10,sample1%>%
                       filter(address==january_data[i,"address"])%>%
                       summarise(
                         address_zip5_distinct_10=n_distinct(zip5)-1,
                         address_zip3_distinct_10 = n_distinct(zip3)-1,
                         address_homephone_distinct_10=n_distinct(homephone)-1,
                         address_ssn_distinct_10 = n_distinct(ssn)-1,
                         address_fullnameDOB_distinct_10 = n_distinct(fullnameDOB)-1,
                         address_fullnamePhone_distinct_10 = n_distinct(fullnamePhone)-1,
                         address_fullnamezip5_distinct_10 = n_distinct(fullnamezip5)-1,
                         address_fullnamessn_distinct_10 = n_distinct(fullnamessn)-1
                       )
  )
  dfzip5_10=rbind(dfzip5_10,sample1%>%
                    filter(zip5==january_data[i,"zip5"])%>%
                    summarise(
                      zip5_ssn_distinct_10=n_distinct(ssn)-1,
                      zip5_homephone_distinct_10=n_distinct(homephone)-1,
                      zip5_address_distinct_10 = n_distinct(address)-1,
                      zip5_fullnameDOB_distinct_10 = n_distinct(fullnameDOB)-1,
                      zip5_fullnamePhone_distinct_10 = n_distinct(fullnamePhone)-1,
                      zip5_fullnamezip5_distinct_10 = n_distinct(fullnamezip5)-1,
                      zip5_fullnamessn_distinct_10 = n_distinct(fullnamessn)-1
                    )
  )

  dfphone_10=rbind(dfphone_10,sample1%>%
                     filter(homephone==january_data[i,"homephone"])%>%
                     summarise(
                       phone_zip5_distinct_10=n_distinct(zip5)-1,
                       phone_address_distinct_10=n_distinct(address)-1,
                       phone_ssn_distinct_10 = n_distinct(ssn)-1,
                       phone_fullnameDOB_distinct_10 = n_distinct(fullnameDOB)-1,
                       phone_fullnamePhone_distinct_10 = n_distinct(fullnamePhone)-1,
                       phone_fullnamezip5_distinct_10 = n_distinct(fullnamezip5)-1,
                       phone_fullnamessn_distinct_10 = n_distinct(fullnamessn)-1
                     )
  )
  dffullnameDOB_10=rbind(dffullnameDOB_10,sample1%>%
                           filter(fullnameDOB==january_data[i,"fullnameDOB"])%>%
                           summarise(
                             fullnameDOB_zip5_distinct_10=n_distinct(zip5)-1,
                             fullnameDOB_address_distinct_10=n_distinct(address)-1,
                             fullnameDOB_ssn_distinct_10 = n_distinct(ssn)-1,
                             fullnameDOB_phone_distinct_10 = n_distinct(homephone)-1,
                             fullnameDOB_fullnamePhone_distinct_10 = n_distinct(fullnamePhone)-1,
                             fullnameDOB_fullnamezip5_distinct_10 = n_distinct(fullnamezip5)-1,
                             fullnameDOB_fullnamessn_distinct_10 = n_distinct(fullnamessn)-1
                           )
  )
  dffullnamessn_10=rbind(dffullnamessn_10,sample1%>%
                           filter(fullnamessn==january_data[i,"fullnamessn"])%>%
                           summarise(
                             fullnamessn_zip5_distinct_10=n_distinct(zip5)-1,
                             fullnamessn_address_distinct_10=n_distinct(address)-1,
                             fullnamessn_ssn_distinct_10 = n_distinct(ssn)-1,
                             fullnamessn_phone_distinct_10 = n_distinct(homephone)-1,
                             fullnamessn_fullnamePhone_distinct_10 = n_distinct(fullnamePhone)-1,
                             fullnamessn_fullnamezip5_distinct_10 = n_distinct(fullnamezip5)-1,
                             fullnamessn_fullnameDOB_distinct_10 = n_distinct(fullnameDOB)-1
                           )
  )
  
  #
  ##for 20 days
  sample1 = sample_record%>%
    filter(date >= max(date_begin,sample_record[i,"date"]-19) & date<=sample_record[i,"date"])
  df_full[i,"ssn_count_20"] = sum(sample1$ssn == january_data[i,"ssn"])-1
  df_full[i,"address_count_20"]=sum(sample1$address==january_data[i,"address"])-1
  df_full[i,"zip5_count_20"]=sum(sample1$zip5==january_data[i,"zip5"])-1
  df_full[i,"homephone_count_20"]=sum(sample1$homephone==january_data[i,"homephone"])-1
  df_full[i,"fullnameDOB_count_20"]=sum(sample1$fullnameDOB==january_data[i,"fullnameDOB"])-1
  df_full[i,"lastnameDOB_count_20"]=sum(sample1$lastnameDOB==january_data[i,"lastnameDOB"])-1
  df_full[i,"firstnameDOB_count_20"]=sum(sample1$firstnameDOB==january_data[i,"firstnameDOB"])-1
  df_full[i,"fullnamePhone_count_20"]=sum(sample1$fullnamePhone==january_data[i,"fullnamePhone"])-1
  df_full[i,"firstnamePhone_count_20"]=sum(sample1$firstnamePhone==january_data[i,"firstnamePhone"])-1
  df_full[i,"lastnamePhone_count_20"]=sum(sample1$lastnamePhone==january_data[i,"lastnamePhone"])-1
  df_full[i,"fullnamezip5_count_20"]=sum(sample1$fullnamezip5==january_data[i,"fullnamezip5"])-1
  df_full[i,"firstnamezip5_count_20"]=sum(sample1$firstnamezip5==january_data[i,"firstnamezip5"])-1
  df_full[i,"lastnamezip5_count_20"]=sum(sample1$lastnamezip5==january_data[i,"lastnamezip5"])-1
  df_full[i,"fullnamessn_count_20"]=sum(sample1$fullnamessn==january_data[i,"fullnamessn"])-1
  
  #


  dfssn_20=rbind(dfssn_20,sample1%>%
                   filter(ssn==january_data[i,"ssn"])%>%
                   summarise(
                     ssn_zip5_distinct_20=n_distinct(zip5)-1,
                     ssn_homephone_distinct_20=n_distinct(homephone)-1,
                     ssn_address_distinct_20 = n_distinct(address)-1,
                     ssn_fullnameDOB_distinct_20 = n_distinct(fullnameDOB)-1,
                     ssn_fullnamePhone_distinct_20 = n_distinct(fullnamePhone)-1,
                     ssn_fullnamezip5_distinct_20 = n_distinct(fullnamezip5)-1,
                     ssn_fullnamessn_distinct_20 = n_distinct(fullnamessn)-1
                   )
  )
  dfaddress_20=rbind(dfaddress_20,sample1%>%
                       filter(address==january_data[i,"address"])%>%
                       summarise(
                         address_zip5_distinct_20=n_distinct(zip5)-1,
                         address_homephone_distinct_20=n_distinct(homephone)-1,
                         address_ssn_distinct_20 = n_distinct(ssn)-1,
                         address_fullnameDOB_distinct_20 = n_distinct(fullnameDOB)-1,
                         address_fullnamePhone_distinct_20 = n_distinct(fullnamePhone)-1,
                         address_fullnamezip5_distinct_20 = n_distinct(fullnamezip5)-1,
                         address_fullnamessn_distinct_20 = n_distinct(fullnamessn)-1
                       )
  )
  dfzip5_20=rbind(dfzip5_20,sample1%>%
                    filter(zip5==january_data[i,"zip5"])%>%
                    summarise(
                      zip5_ssn_distinct_20=n_distinct(ssn)-1,
                      zip5_homephone_distinct_20=n_distinct(homephone)-1,
                      zip5_address_distinct_20 = n_distinct(address)-1,
                      zip5_fullnameDOB_distinct_20 = n_distinct(fullnameDOB)-1,
                      zip5_fullnamePhone_distinct_20 = n_distinct(fullnamePhone)-1,
                      zip5_fullnamezip5_distinct_20 = n_distinct(fullnamezip5)-1,
                      zip5_fullnamessn_distinct_20 = n_distinct(fullnamessn)-1
                    )
  )

  dfphone_20=rbind(dfphone_20,sample1%>%
                     filter(homephone==january_data[i,"homephone"])%>%
                     summarise(
                       phone_zip5_distinct_20=n_distinct(zip5)-1,
                       phone_address_distinct_20=n_distinct(address)-1,
                       phone_ssn_distinct_20 = n_distinct(ssn)-1,
                       phone_fullnameDOB_distinct_20 = n_distinct(fullnameDOB)-1,
                       phone_fullnamePhone_distinct_20 = n_distinct(fullnamePhone)-1,
                       phone_fullnamezip5_distinct_20 = n_distinct(fullnamezip5)-1,
                       phone_fullnamessn_distinct_20 = n_distinct(fullnamessn)-1
                     )
  )
  dffullnameDOB_20=rbind(dffullnameDOB_20,sample1%>%
                           filter(fullnameDOB==january_data[i,"fullnameDOB"])%>%
                           summarise(
                             fullnameDOB_zip5_distinct_20=n_distinct(zip5)-1,
                             fullnameDOB_address_distinct_20=n_distinct(address)-1,
                             fullnameDOB_ssn_distinct_20 = n_distinct(ssn)-1,
                             fullnameDOB_phone_distinct_20 = n_distinct(homephone)-1,
                             fullnameDOB_fullnamePhone_distinct_20 = n_distinct(fullnamePhone)-1,
                             fullnameDOB_fullnamezip5_distinct_20 = n_distinct(fullnamezip5)-1,
                             fullnameDOB_fullnamessn_distinct_20 = n_distinct(fullnamessn)-1
                           )
  )
  dffullnamessn_20=rbind(dffullnamessn_20,sample1%>%
                           filter(fullnamessn==january_data[i,"fullnamessn"])%>%
                           summarise(
                             fullnamessn_zip5_distinct_20=n_distinct(zip5)-1,
                             fullnamessn_address_distinct_20=n_distinct(address)-1,
                             fullnamessn_ssn_distinct_20 = n_distinct(ssn)-1,
                             fullnamessn_phone_distinct_20 = n_distinct(homephone)-1,
                             fullnamessn_fullnamePhone_distinct_20 = n_distinct(fullnamePhone)-1,
                             fullnamessn_fullnamezip5_distinct_20 = n_distinct(fullnamezip5)-1,
                             fullnamessn_fullnameDOB_distinct_20 = n_distinct(fullnameDOB)-1
                           )
  )
  
  ##for 30 days
  sample1 = sample_record%>%
    filter(date >= max(date_begin,sample_record[i,"date"]-29) & date<=sample_record[i,"date"])
  df_full[i,"ssn_count_30"] = sum(sample1$ssn == january_data[i,"ssn"])-1
  df_full[i,"address_count_30"]=sum(sample1$address==january_data[i,"address"])-1
  df_full[i,"zip5_count_30"]=sum(sample1$zip5==january_data[i,"zip5"])-1
  df_full[i,"homephone_count_30"]=sum(sample1$homephone==january_data[i,"homephone"])-1
  df_full[i,"fullnameDOB_count_30"]=sum(sample1$fullnameDOB==january_data[i,"fullnameDOB"])-1
  df_full[i,"lastnameDOB_count_30"]=sum(sample1$lastnameDOB==january_data[i,"lastnameDOB"])-1
  df_full[i,"firstnameDOB_count_30"]=sum(sample1$firstnameDOB==january_data[i,"firstnameDOB"])-1
  df_full[i,"fullnamePhone_count_30"]=sum(sample1$fullnamePhone==january_data[i,"fullnamePhone"])-1
  df_full[i,"firstnamePhone_count_30"]=sum(sample1$firstnamePhone==january_data[i,"firstnamePhone"])-1
  df_full[i,"lastnamePhone_count_30"]=sum(sample1$lastnamePhone==january_data[i,"lastnamePhone"])-1
  df_full[i,"fullnamezip5_count_30"]=sum(sample1$fullnamezip5==january_data[i,"fullnamezip5"])-1
  df_full[i,"firstnamezip5_count_30"]=sum(sample1$firstnamezip5==january_data[i,"firstnamezip5"])-1
  df_full[i,"lastnamezip5_count_30"]=sum(sample1$lastnamezip5==january_data[i,"lastnamezip5"])-1
  df_full[i,"fullnamessn_count_30"]=sum(sample1$fullnamessn==january_data[i,"fullnamessn"])-1
  
  dfssn_30=rbind(dfssn_30,sample1%>%
                   filter(ssn==january_data[i,"ssn"])%>%
                   summarise(
                     ssn_zip5_distinct_30=n_distinct(zip5)-1,
                     ssn_homephone_distinct_30=n_distinct(homephone)-1,
                     ssn_address_distinct_30 = n_distinct(address)-1,
                     ssn_fullnameDOB_distinct_30 = n_distinct(fullnameDOB)-1,
                     ssn_fullnamePhone_distinct_30 = n_distinct(fullnamePhone)-1,
                     ssn_fullnamezip5_distinct_30 = n_distinct(fullnamezip5)-1,
                     ssn_fullnamessn_distinct_30 = n_distinct(fullnamessn)-1
                   )
  )
  dfaddress_30=rbind(dfaddress_30,sample1%>%
                       filter(address==january_data[i,"address"])%>%
                       summarise(
                         address_zip5_distinct_30=n_distinct(zip5)-1,
                         address_homephone_distinct_30=n_distinct(homephone)-1,
                         address_ssn_distinct_30 = n_distinct(ssn)-1,
                         address_fullnameDOB_distinct_30 = n_distinct(fullnameDOB)-1,
                         address_fullnamePhone_distinct_30 = n_distinct(fullnamePhone)-1,
                         address_fullnamezip5_distinct_30 = n_distinct(fullnamezip5)-1,
                         address_fullnamessn_distinct_30 = n_distinct(fullnamessn)-1
                       )
  )
  dfzip5_30=rbind(dfzip5_30,sample1%>%
                    filter(zip5==january_data[i,"zip5"])%>%
                    summarise(
                      zip5_ssn_distinct_30=n_distinct(ssn)-1,
                      zip5_homephone_distinct_30=n_distinct(homephone)-1,
                      zip5_address_distinct_30 = n_distinct(address)-1,
                      zip5_fullnameDOB_distinct_30 = n_distinct(fullnameDOB)-1,
                      zip5_fullnamePhone_distinct_30 = n_distinct(fullnamePhone)-1,
                      zip5_fullnamezip5_distinct_30 = n_distinct(fullnamezip5)-1,
                      zip5_fullnamessn_distinct_30 = n_distinct(fullnamessn)-1
                    )
  )

  dfphone_30=rbind(dfphone_30,sample1%>%
                     filter(homephone==january_data[i,"homephone"])%>%
                     summarise(
                       phone_zip5_distinct_30=n_distinct(zip5)-1,
                       phone_address_distinct_30=n_distinct(address)-1,
                       phone_ssn_distinct_30 = n_distinct(ssn)-1,
                       phone_fullnameDOB_distinct_30 = n_distinct(fullnameDOB)-1,
                       phone_fullnamePhone_distinct_30 = n_distinct(fullnamePhone)-1,
                       phone_fullnamezip5_distinct_30 = n_distinct(fullnamezip5)-1,
                       phone_fullnamessn_distinct_30 = n_distinct(fullnamessn)-1
                     )
  )
  dffullnameDOB_30=rbind(dffullnameDOB_30,sample1%>%
                           filter(fullnameDOB==january_data[i,"fullnameDOB"])%>%
                           summarise(
                             fullnameDOB_zip5_distinct_30=n_distinct(zip5)-1,
                             fullnameDOB_address_distinct_30=n_distinct(address)-1,
                             fullnameDOB_ssn_distinct_30 = n_distinct(ssn)-1,
                             fullnameDOB_phone_distinct_30 = n_distinct(homephone)-1,
                             fullnameDOB_fullnamePhone_distinct_30 = n_distinct(fullnamePhone)-1,
                             fullnameDOB_fullnamezip5_distinct_30 = n_distinct(fullnamezip5)-1,
                             fullnameDOB_fullnamessn_distinct_30 = n_distinct(fullnamessn)-1
                           )
  )
  dffullnamessn_30=rbind(dffullnamessn_30,sample1%>%
                           filter(fullnamessn==january_data[i,"fullnamessn"])%>%
                           summarise(
                             fullnamessn_zip5_distinct_30=n_distinct(zip5)-1,
                             fullnamessn_address_distinct_30=n_distinct(address)-1,
                             fullnamessn_ssn_distinct_30 = n_distinct(ssn)-1,
                             fullnamessn_phone_distinct_30 = n_distinct(homephone)-1,
                             fullnamessn_fullnamePhone_distinct_30 = n_distinct(fullnamePhone)-1,
                             fullnamessn_fullnamezip5_distinct_30 = n_distinct(fullnamezip5)-1,
                             fullnamessn_fullnameDOB_distinct_30 = n_distinct(fullnameDOB)-1
                           )
  )
  
print(i)
}

sample1=january_data[1:6604,]



df1=data.frame()
df1 = rbind(df1,january_data%>%
  filter(ssn==737610282)%>%
    group_by(ssn)%>%
    summarise(count1=n_distinct(zip5),count2 = n_distinct(zip3))%>%
    select(count1:count2)
  
  )
  

january_data%>%
  filter(record==6604) ->totest


january_data%>%
  filter(date>=january_data[6604,"date"]-2 & date<=january_data[6604,"date"])-> totest
totest%>%
  filter(ssn==279646463)%>%
  group_by(ssn)%>%
  summarize(count=n_distinct(zip5))
  

january_data%>%
  group_by(ssn)%>%
  summarise(count=n()) ->nikhil
max(date_begin+1,date_begin-2)


##check the algo
#1719 record number
january_data%>%
  filter(record==4577)
##737610282, 97256
##date = 01/09/2016

##instead of 3 it should be 2 days before
january_data%>%
  filter(date>= january_data[4577,"date"]-3 & date<=january_data[4577,"date"]) %>%
  group_by(zip5)%>%
  summarize(count=n()-1) -> nikhil



############testing
i=6604

  sample_record = january_data%>%
    filter(record<=january_data[i,"record"])
  #print(nrow(sample_record))
  ##for 3 day window
  sample1 = sample_record%>%
    filter(date >= max(date_begin,sample_record[i,"date"]-2) & date<=sample_record[i,"date"])
  df3=data.frame()
  df3[1,c("ssn_zip5_distinct_3","ssn_zip3_distinct_3","ssn_homephone_distinct_3","ssn_address_distinct_3","ssn_fullnameDOB_distinct_3","ssn_fullnamePhone_distinct_3","ssn_fullnamezip5_distinct_3","ssn_fullnamessn_distinct_3")]=sample1%>%
    df3[1,c("ssn_count","ssn_dis")]=matrix(sample1%>%
    filter(ssn==january_data[i,"ssn"])%>%
    summarise(
      count1=n_distinct(zip5)-1,
      count2 = n_distinct(zip3)-1
      # count3=n_distinct(homephone)-1,
      # count4 = n_distinct(address)-1,
      # count5 = n_distinct(fullnameDOB)-1,
      # count6 = n_distinct(fullnamePhone)-1,
      # count7 = n_distinct(fullnamezip5)-1,
      # count8 = n_distinct(fullnamessn)-1
    ),ncol=2,byrow=TRUE)
  # df[i,"ssn_count_3"] = sum(sample1$ssn == january_data[i,"ssn"])-1
  # df[i,"zip5_count_3"]=sum(sample1$zip5==january_data[i,"zip5"])-1
  # df[i,"homephone_count_3"]=sum(sample1$homephone==january_data[i,"homephone"])-1
  # df[i,"zip3_count_3"]=sum(sample1$zip3==january_data[i,"zip3"])-1
  # df[i,"fullnameDOB_count_3"]=sum(sample1$fullnameDOB==january_data[i,"fullnameDOB"])-1
  # df[i,"lastnameDOB_count_3"]=sum(sample1$lastnameDOB==january_data[i,"lastnameDOB"])-1
  # df[i,"firstnameDOB_count_3"]=sum(sample1$firstnameDOB==january_data[i,"firstnameDOB"])-1
  # df[i,"fullnamePhone_count_3"]=sum(sample1$fullnamePhone==january_data[i,"fullnamePhone"])-1
  # df[i,"firstnamePhone_count_3"]=sum(sample1$firstnamePhone==january_data[i,"firstnamePhone"])-1
  # df[i,"lastnamePhone_count_3"]=sum(sample1$lastnamePhone==january_data[i,"lastnamePhone"])-1
  # df[i,"fullnamezip5_count_3"]=sum(sample1$fullnamezip5==january_data[i,"fullnamezip5"])-1
  # df[i,"firstnamezip5_count_3"]=sum(sample1$firstnamezip5==january_data[i,"firstnamezip5"])-1
  # df[i,"lastnamezip5_count_3"]=sum(sample1$lastnamezip5==january_data[i,"lastnamezip5"])-1
  
  ##for ssn count distinct occurences of other variables
  
  
  df1 = rbind(df1,
              
              
              
              sample1%>%
                filter(ssn==january_data[i,"ssn"])%>%
                summarise(ssn_zip5_distinct_3=n_distinct(zip5)-1,
                          ssn_zip3_distinct_3 = n_distinct(zip3)-1,
                          ssn_homephone_distinct_3=n_distinct(homephone)-1,
                          ssn_fullnameDOB_distinct_3 = n_distinct(fullnameDOB)-1,
                          ssn_firstnameDOB_distinct_3 = n_distinct(firstnameDOB)-1,
                          ssn_lastnameDOB_distinct_3 = n_distinct(lastnameDOB)-1,
                          ssn_fullnamePhone_distinct_3 = n_distinct(fullnamePhone)-1,
                          ssn_firstnamePhone_distinct_3 = n_distinct(firstnamePhone)-1,
                          ssn_lastnamePhone_distinct_3 = n_distinct(lastnamePhone)-1,
                          ssn_fullnamezip5_distinct_3 = n_distinct(fullnamezip5)-1,
                          ssn_firstnamezip5_distinct_3 = n_distinct(firstnamezip5)-1,
                          ssn_lastnamezip5_distinct_3 = n_distinct(lastnamezip5)-1
                ))
  df1=data.frame()
  print(i) 
  
  
df_allvariables_from75238 = df_full  
df_allvariables_from75238 = df_allvariables_from75238[75238:94866,]  
# dfaddress_3=dfaddress_3[1:75237,]
# dfzip5_3=dfzip5_3[1:75237,]
df1=data.frame()
##combine all variables together
df1=cbind(january_data$record[75238:94866],january_data$fraud[75238:94866],january_data$date[75238:94866],df_allvariables_from75238)
df1=cbind(df1,dfaddress_3,dfaddress_10,dfaddress_20,dfaddress_30)
df1=cbind(df1,dffullnameDOB_3,dffullnameDOB_10,dffullnameDOB_20,dffullnameDOB_30)
df1 = cbind(df1,dffullnamessn_3,dffullnamessn_10,dffullnamessn_20,dffullnamessn_30)
df1=cbind(df1,dfphone_3,dfphone_10,dfphone_20,dfphone_30)
df1 = cbind(df1,dfssn_3, dfssn_10,dfssn_20,dfssn_30 )
df1 = cbind(df1,dfzip5_3, dfzip5_10,dfzip5_20,dfzip5_30 )

##change the column names
colnames(df_allvariable_75237)[1:3]=c("record","fraud","date")
colnames(df_allvariables_from75238)[1:3]=c("record","fraud","date")

write.csv(df_allvariables_from75238,"allvariables_from75238.csv")
df_allvariables_from75238=df1
df10=df_full
df_allvariable_75237 = df1
df_allvariables = rbind(df_allvariable_75237,df_allvariables_from75238)
write.csv(df_allvariables,"all_variables_228.csv")

january_data[90325,"ssn"]
check = january_data%>%
  filter(record<=90325)
check%>%
  filter(date>=check$date[90325]-9 & date<=check$date[90325]) ->check1
sample1%>%
  filter(fullnameDOB==january_data[90325,"fullnameDOB"])%>%
  summarise(
    fullnameDOB_zip5_distinct_30=n_distinct(zip5)-1,
    fullnameDOB_address_distinct_30=n_distinct(address)-1,
    fullnameDOB_ssn_distinct_30 = n_distinct(ssn)-1,
    fullnameDOB_phone_distinct_30 = n_distinct(homephone)-1,
    fullnameDOB_fullnamePhone_distinct_30 = n_distinct(fullnamePhone)-1,
    fullnameDOB_fullnamezip5_distinct_30 = n_distinct(fullnamezip5)-1,
    fullnameDOB_fullnamessn_distinct_30 = n_distinct(fullnamessn)-1
  )
    
##another check
check = january_data%>%
  filter(record<=78388)

check%>%
  filter(date>=check$date[78388]-29 & date<=check$date[78388]) -> check1
check1%>%
  filter(fullnameDOB==january_data[78388,"fullnameDOB"])%>%
  summarise(
    #fullnameDOB_zip5_distinct_30=n_distinct(zip5)-1
    # fullnameDOB_address_distinct_30=n_distinct(address)-1,
    # fullnameDOB_ssn_distinct_30 = n_distinct(ssn)-1,
     #fullnameDOB_phone_distinct_30 = n_distinct(homephone)-1
    # fullnameDOB_fullnamePhone_distinct_30 = n_distinct(fullnamePhone)-1,
     fullnameDOB_fullnamezip5_distinct_30 = n_distinct(fullnamezip5)-1,
    # fullnameDOB_fullnamessn_distinct_30 = n_distinct(fullnamessn)-1)
  )
