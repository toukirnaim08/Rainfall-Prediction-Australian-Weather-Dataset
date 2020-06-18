library(dplyr)
library(lubridate)
library(tidyverse)
library("Hmisc")
library(corrplot)
library(ggplot2)
library(InformationValue)
library(imputeTS)
library('caret')


currentDirectory <-dirname(rstudioapi::getSourceEditorContext()$path)
filepathDir<- paste(currentDirectory,"/weatherAUS.csv",sep = "")
#yelpData <- read.csv(filepathDir, row.names=NULL)
df<-read.csv(filepathDir)
table(df$Location)

##############################
#######Data Preprocessing#####
##############################
df <- df%>%mutate(RainToday = ifelse(RainToday == "No",0,1))
df <- df%>%mutate(RainTomorrow = ifelse(RainTomorrow == "No",0,1))
df<-df%>%mutate(Date=as.Date(df$Date, format="%Y-%m-%d"))

#Check date covered
min(df$Date)
max(df$Date)

#Create ranking and assign to all categorical values
WindDir3pmTemp <- df$WindDir3pm
WindDir3pmTemp<-transform(WindDir3pmTemp,id=as.numeric(factor(WindDir3pmTemp)))
df$WindDir3pm = WindDir3pmTemp$id

WindDir9amTemp <- df$WindDir9am
WindDir9amTemp<-transform(WindDir9amTemp,id=as.numeric(factor(WindDir9amTemp)))
df$WindDir9am = WindDir9amTemp$id

WindGustDirTemp <- df$WindGustDir
WindGustDirTemp<-transform(WindGustDirTemp,id=as.numeric(factor(WindGustDirTemp)))
df$WindGustDir = WindGustDirTemp$id

remove(WindDir3pmTemp, WindDir9amTemp,WindGustDirTemp)

#replace NA with average
df<-na_mean(df)
df$RainToday<- as.integer(df$RainToday)
View(df)
dim(df)
##############################
#####End of Preprocessing#####
##############################



################################
#####Feature Selection##########
################################
#correlation matrix
df_corr<-cor(df[,3:22],method = c("spearman"))
corrplot(df_corr,method = "number", is.corr=FALSE)
hc = findCorrelation(df_corr  , cutoff=0.7) # putt any value as a "cutoff" 
hc = sort(hc)
reduced_Data = df_corr[,-c(hc)]
colnames(reduced_Data)


new_df<-df
new_df<-new_df%>%mutate(DailyTempMean=(MaxTemp + MinTemp)/2)
new_df<-new_df%>%mutate(DailyPressMean=(Pressure9am + Pressure3pm)/2)
final_df<-new_df%>%select(-c(3,4,5,16,17,20,21,23))
final_df.cor<-cor(final_df[,3:18], method = c("spearman"))
corrplot(final_df.cor,method = "number", is.corr=FALSE)
View(final_df)
dim(final_df)
################################
####End of Feature Selection####
################################