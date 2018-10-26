library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(stringi)
library(tidyr)
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

##record field
max(dqr_data$record)
##date field
length(unique(dqr_data$date))
max(dqr_data$date)
ggplot(dqr_data,aes(x=date))+
  geom_histogram(bins=365,color="black")+
  ylab("# of applications")+
  xlab("Date of Application")

##ssn field
length(unique(dqr_data$ssn))
dqr_data$ssn=as.factor(dqr_data$ssn)
dqr_data%>%
  filter(ssn==611753360)%>%
  nrow()

dqr_data%>%
  group_by(ssn)%>%
  summarize(count=n())%>%
  arrange(-count)%>%
  head(25)%>%
  ggplot(aes(x=reorder(ssn,-count),y=count))+
  geom_bar(stat="identity")+
  scale_y_log10()+
  ylab("Log10Count")+
  xlab("Social Security Number")+
  ggtitle("Top 25 SSN")+
  coord_flip()

##firstname
length(unique(dqr_data$firstname))
dqr_data%>%
  group_by(firstname)%>%
  summarize(count=n())%>%
  arrange(-count)%>%
  head(25)%>%
  ggplot(aes(x=reorder(firstname,-count),y=count))+
  geom_bar(stat="identity")+
  scale_y_log10()+
  ylab("Log10Count")+
  xlab("First Name")+
  ggtitle("Top 25 First Names")+
  coord_flip() 

#lastname
length(unique(dqr_data$lastname))
dqr_data%>%
  group_by(lastname)%>%
  summarize(count=n())%>%
  arrange(-count)%>%
  head(25)%>%
  ggplot(aes(x=reorder(lastname,-count),y=count))+
  geom_bar(stat="identity")+
  scale_y_log10()+
  ylab("Log10Count")+
  xlab("Last Name")+
  ggtitle("Top 25 Last Names")+
  coord_flip()

#address
length(unique(dqr_data$address))
dqr_data%>%
  group_by(address)%>%
  summarize(count=n())%>%
  arrange(-count)%>%
  head(25)%>%
  ggplot(aes(x=reorder(address,-count),y=count))+
  geom_bar(stat="identity")+
  scale_y_log10()+
  ylab("Log10Count")+
  xlab("Address")+
  ggtitle("Top 25 Addresses")+
  coord_flip()

#zip5
length(unique(dqr_data$zip5))
dqr_data%>%
  group_by(zip5)%>%
  summarize(count=n())%>%
  arrange(-count)%>%
  head(25)%>%
  ggplot(aes(x=reorder(zip5,-count),y=count))+
  geom_bar(stat="identity")+
  scale_y_log10()+
  ylab("Log10Count")+
  xlab("Zip5")+
  ggtitle("Top 25 Zip Codes")+
  coord_flip()

#dob
length(unique(dqr_data$dob))
dqr_data$dob=as.character(dqr_data$dob)
dqr_data$dob
max(dqr_data$dob)
min(dqr_data$dob)
dqr_data%>%
  group_by(dob)%>%
  summarize(count=n())%>%
  arrange(-count)%>%
  head(25)%>%
  ggplot(aes(x=reorder(dob,-count),y=count))+
  geom_bar(stat="identity")+
  scale_y_log10()+
  ylab("Log10Count")+
  xlab("Date of Birth")+
  ggtitle("Top 25 DOBs")+
  coord_flip()

#homephone
length(unique(dqr_data$homephone))
dqr_data%>%
  group_by(homephone)%>%
  summarize(count=n())%>%
  arrange(-count)%>%
  head(25)%>%
  ggplot(aes(x=reorder(homephone,-count),y=count))+
  geom_bar(stat="identity")+
  scale_y_log10()+
  ylab("Log10Count")+
  xlab("Home Phone Number")+
  ggtitle("Top 25 Home Phone Numbers")+
  coord_flip()

#Fraud
length(unique(dqr_data$fraud))
dqr_data$fraud=as.factor(dqr_data$fraud)
dqr_data%>%
  group_by(fraud)%>%
  summarize(count=n())%>%
  mutate(fraud=if_else(fraud==1,"Fraud","No Fraud"))%>%
  ggplot(aes(x=fraud,y=count))+
  geom_bar(stat="identity")+
  ylab("Number of Records")+
  xlab("")+
  ggtitle("Distribution of Fraud Records")
