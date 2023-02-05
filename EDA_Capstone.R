#EDA Capstone project MSDS 498 - Gautam Kulkarni
# Libraries
library("tidyverse")
library("lubridate")
library("ggplot2")
library("summarytools")


list_csv_files <- list.files(path = "/Users/gautamkulkarni/Documents/MSDS/Capstone/DataSet/", pattern='data*.csv',full.names = TRUE)
ukDataSet=do.call(rbind, lapply(list_csv_files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#find duplicate charging event id
nOccur<-data.frame(table(ukDataSet$Charging.event))
nOccur[nOccur$Freq > 1,]

#remove duplicates
ukDataSet<-distinct(ukDataSet,ukDataSet$Charging.event,.keep_all = TRUE)
ukDataSet<-ukDataSet[-10]
#rename cols - Replace . with _
names(ukDataSet) <- gsub(x = names(ukDataSet), pattern = "\\.", replacement = "_") 

ukDataSetClean<-ukDataSet
#convert char to timestamp
ukDataSetClean$Plug_in_Date_and_Time<-strptime(ukDataSetClean$Plug_in_Date_and_Time, "%d/%m/%Y %H:%M",tz="Europe/London")
ukDataSetClean$Unplug_Date_and_Time<-strptime(ukDataSetClean$Unplug_Date_and_Time, "%d/%m/%Y %H:%M",tz="Europe/London")
ukDataSetClean$Charge_start_Date_and_Time<-strptime(ukDataSetClean$Charge_start_Date_and_Time, "%d/%m/%Y %H:%M",tz="Europe/London")
ukDataSetClean$Charge_end_Date_and_Time<-strptime(ukDataSetClean$Charge_end_Date_and_Time, "%d/%m/%Y %H:%M",tz="Europe/London")
#view summary for uk dataset
view(dfSummary(ukDataSetClean$Plug_in_Date_and_Time))

#add charge hours column
ukDataSetClean$ChargingHours<-as.numeric(difftime(ukDataSetClean$Charge_end_Date_and_Time,ukDataSetClean$Charge_start_Date_and_Time,units="hours") )
#summary
summary(ukDataSetClean$ChargingHours)
hist(ukDataSetClean$ChargingHours)
boxplot(ukDataSetClean$ChargingHours)

#remove events greater than 24 hours
ukDataSetClean<-subset(ukDataSetClean,ukDataSetClean$ChargingHours<24)

#charge start Hour 
ukDataSetClean$ChargeStartHourOfDay<-as.numeric(format(ukDataSetClean$Charge_start_Date_and_Time,format = "%H"))
ukDataSetClean$ChargeEndHourOfDay<-as.numeric(format(ukDataSetClean$Charge_end_Date_and_Time,format = "%H"))

######## Colorado Dataset ################
coloDataSet <- read_csv('/Users/gautamkulkarni/Documents/MSDS/Capstone/DataSet/ColoradoDataSet.csv')

coloDataSet$Charge_start_Date_and_Time<-strptime(coloDataSet$Charge_start_Date_and_Time, "%m/%d/%Y %H:%M")
coloDataSet$Charge_end_Date_and_Time<-strptime(coloDataSet$Charge_end_Date_and_Time, "%m/%d/%Y %H:%M")
coloDataSet$ChargingHours<-as.numeric(difftime(coloDataSet$Charge_end_Date_and_Time,coloDataSet$Charge_start_Date_and_Time,units="hours") )

#summary
summary(coloDataSet$ChargingHours)
hist(coloDataSet$ChargingHours)
boxplot(coloDataSet$ChargingHours)

#remove events greater than 24 hours
coloDataSet<-subset(coloDataSet,coloDataSet$ChargingHours<24)

#charge start Hour 
coloDataSet$ChargeStartHourOfDay<-as.numeric(format(coloDataSet$Charge_start_Date_and_Time,format = "%H"))
coloDataSet$ChargeEndHourOfDay<-as.numeric(format(coloDataSet$Charge_end_Date_and_Time,format = "%H"))

#merge data
ukdata<-ukDataSetClean[-c(4,5,6)]
