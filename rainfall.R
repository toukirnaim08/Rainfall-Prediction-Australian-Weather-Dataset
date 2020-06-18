library(dplyr)
library(lubridate)
library(tidyverse)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(InformationValue)
library(imputeTS)
library(caret)
library(pROC)


currentDirectory <-dirname(rstudioapi::getSourceEditorContext()$path)
filepathDir<- paste(currentDirectory,"/weatherAUS.csv",sep = "")
#df<-read.csv("weatherAUS.csv")
df<-read.csv(filepathDir)
table(df$Location)
table(is.na(df))
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

new_df<-df
new_df<-new_df%>%mutate(DailyTempMean=(MaxTemp + MinTemp)/2)
new_df<-new_df%>%mutate(DailyPressMean=(Pressure9am + Pressure3pm)/2)
final_df<-new_df%>%select(-c(3,4,5,16,17,20,21,23))
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


final_df.cor<-cor(final_df[,3:18], method = c("spearman"))
corrplot(final_df.cor,method = "number", is.corr=FALSE)
################################
####End of Feature Selection####
################################

###############################
######Factor Analysis##########
###############################
factor_df<-final_df%>%select(-c(1,2,16))
df.cas<-cor(factor_df, use="complete.obs")
corrplot(df.cas,method = "number", is.corr=FALSE)
summary(df.cas)
df.pca <- prcomp(factor_df[,3:15], scale=TRUE) 

par(mar=c(5,4,2,2))
screeplot(df.pca, type="lines",pch=19,col='blue')

# It's not obvious from the scree plot.  Let's check the
# eigenvalues and use Kaiser's criterion
# To obtain the eigenvalues, square the sdev output (as explained in Johnson & Wichern)
(df.pca$sdev)^2


fit <- factanal(cov=df.cas, factors=3, rotation="varimax")
print(fit, digits=2, cutoff=.4, sort=TRUE)
# plot factor 1 by factor 2
load <- fit$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(factor_df[,3:15]),cex=.8) # add variable names
###############################
####End of Factor Analysis#####
###############################

################################
###Build  Prediction Model#######
################################
####################################
##########Adelaide area#############
####################################
source("adelaide.R")

############################
#### with outlier result####
############################
#Model 1
summary(adl_1)
#Model 2
summary(adl_2)
#Model 3
summary(adl_3)

#confusion matrix & error with outliers
InformationValue::confusionMatrix(testAdl$RainTomorrow, adl_predicted)
InformationValue::misClassError(testAdl$RainTomorrow, adl_predicted)

#ROC Curve with outliers
rocobj<-roc(testAdl$RainTomorrow, adl_predicted, plot =TRUE, legacy.axes = TRUE, percent = TRUE,
    main="ROC Curve for Adelaide Logistic Regression Model",
    xlab="False Positive Percentage", 
    ylab="True Positive Percentage", lwd=2, print.auc = TRUE)
par(pty="m")
g <- ggroc(rocobj)
ggroc(rocobj, alpha = 0.5, colour = "red", linetype = 2, size = 2)
plt <- ggplot(rocobj, aes(x=FalsePositive, y=TruePositive, color=Curve)) + geom_line()
print(plt)

res<-ROC(form = outcome ~ s100b, data=aSAH, plot="sp" )
lines(res$res$lr,res$res$spec, type="s", col="red") #specificity
lines(res$res$lr,res$res$sens, type="s", col="green") # sensitivity
lines(res$res$lr,res$res$pvp, type="s", col="orange") # pvp
lines(res$res$lr,res$res$pvn, type="s", col="magenta")

rocobj$
ggplot(rocobj,aes(rocobj$sensitivities,rocobj$specificities))+geom_line(size = 2, alpha = 0.7)+
    labs(title= "ROC curve", 
         x = "False Positive Rate (1-Specificity)", 
         y = "True Positive Rate (Sensitivity)")

library(pROC)
roc_rose <- plot(roc(testAdl$RainTomorrow, adl_predicted),
                 main="ROC Curve for Adelaide Logistic Regression Model", 
                 xlab="False Positive Percentage", 
                 ylab="True Positive Percentage",
                 plot =TRUE, legacy.axes = TRUE, percent = TRUE,
                 lwd=2, print.auc = TRUE, col = "blue",col.main="seagreen",
                 col.lab="seagreen",col.axis = 'seagreen')

#to add 1 more roc
#plot.roc(testAdl$RainTomorrow, adl_predicted_no_outlier, legacy.axes = TRUE, percent = TRUE, lwd=4, print.auc = TRUE)

###############################
#### without outlier result####
###############################
summary(adl_no_outlier_3)
#confusion matrix & error with outliers
InformationValue::confusionMatrix(testAdl$RainTomorrow, adl_predicted_no_outlier)
InformationValue::misClassError(testAdl$RainTomorrow, adl_predicted_no_outlier)


#ROC Curve without outliers
roc(testAdl$RainTomorrow, adl_predicted_no_outlier, plot =TRUE, legacy.axes = TRUE, percent = TRUE,
    main="ROC Curve for Adelaide Logistic Regression Model without Outliers",
    xlab="False Positive Percentage", 
    ylab="True Positive Percentage", lwd=2, print.auc = TRUE)
par(pty="m")



#Changed Roc Curve 
plot(roc(testAdl$RainTomorrow, adl_predicted),
         main="ROC Curve for Adelaide Logistic Regression Model", 
         xlab="False Positive Percentage", 
         ylab="True Positive Percentage",
         plot =TRUE, legacy.axes = TRUE, percent = TRUE,
         lwd=2, print.auc = TRUE, col = "red",col.main="seagreen",
         col.lab="seagreen",col.axis = 'seagreen',
     print.auc.y = 0.1)
par(new=TRUE)
plot(roc(testAdl$RainTomorrow, adl_predicted_no_outlier),
         main="", 
         xlab="False Positive Percentage", 
         ylab="True Positive Percentage",
         plot =TRUE, legacy.axes = TRUE, percent = TRUE,
         lwd=2, print.auc = TRUE, col = "blue",col.main="seagreen",
         col.lab="seagreen",col.axis = 'seagreen',
     print.auc.y = 0.2)
legend("topright", 
       legend = c("Without outliers", "With outliers"), 
       col = c('red','blue'), 
       lty = c(1,1), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = c('red','blue'), 
       horiz = F , 
       inset = c(0.1, 0.1))


####################################
###########Brisbane area############
####################################
source("brisbane.R")
#Model 1
summary(logitMod_bris_1)
#Model 2
summary(logitMod_bris_2)
#Model 3
summary(logitMod_bris_3)

#confusion matrix & error
InformationValue::confusionMatrix(testBris$RainTomorrow, bris_predicted)
InformationValue::misClassError(testBris$RainTomorrow, bris_predicted)

#ROC Curve
roc(testBris$RainTomorrow, bris_predicted, plot =TRUE, legacy.axes = TRUE, percent = TRUE, 
    main="ROC Curve for Brisbane  Prediction Model",
    xlab="False Positive Percentage", 
    ylab="True Positive Percentage", lwd=4, print.auc = TRUE)
par(pty="m")

####################################
##########Canberra area#############
####################################
source("canberra.R")
#Model 1
summary(can_1)
#Model 2
summary(can_2)
#Model 3
summary(can_3)

#confusion matrix & error
InformationValue::confusionMatrix(testCan$RainTomorrow, can_predicted)
InformationValue::misClassError(testCan$RainTomorrow, can_predicted)

#ROC Curve
roc(testCan$RainTomorrow, can_predicted, plot =TRUE, legacy.axes = TRUE, percent = TRUE, 
    main="ROC Curve for Canberra  Prediction Model",
    xlab="False Positive Percentage", 
    ylab="True Positive Percentage", lwd=2, print.auc = TRUE)
par(pty="m")


###############################
#### without outlier result####
###############################
summary(can_no_outlier_2)
#confusion matrix & error with outliers
InformationValue::confusionMatrix(testCan$RainTomorrow, can_predicted_no_outlier)
InformationValue::misClassError(testCan$RainTomorrow, can_predicted_no_outlier)


#ROC Curve without outliers
roc(testCan$RainTomorrow, can_predicted_no_outlier, plot =TRUE, legacy.axes = TRUE, percent = TRUE,
    main="ROC Curve for Canberra  Prediction Model without Outliers",
    xlab="False Positive Percentage", 
    ylab="True Positive Percentage", lwd=2, print.auc = TRUE)
par(pty="m")

#changed Roc Curve
plot(roc(testCan$RainTomorrow, can_predicted),
     main="ROC Curve for Canberra Logistic Regression Model", 
     xlab="False Positive Percentage", 
     ylab="True Positive Percentage",
     plot =TRUE, legacy.axes = TRUE, percent = TRUE,
     lwd=2, print.auc = TRUE, col = "red",col.main="seagreen",
     col.lab="seagreen",col.axis = 'seagreen',
     print.auc.y = 0.1)
par(new=TRUE)
plot(roc(testCan$RainTomorrow, can_predicted_no_outlier),
     main="", 
     xlab="False Positive Percentage", 
     ylab="True Positive Percentage",
     plot =TRUE, legacy.axes = TRUE, percent = TRUE,
     lwd=2, print.auc = TRUE, col = "blue",col.main="seagreen",
     col.lab="seagreen",col.axis = 'seagreen',
     print.auc.y = 0.2)
legend("topright", 
       legend = c("Without outliers", "With outliers"), 
       col = c('red','blue'), 
       lty = c(1,1), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = c('red','blue'), 
       horiz = F , 
       inset = c(0.1, 0.1))


####################################
############Darwin area#############
####################################
source("darwin.R")
#Model 1
summary(logitMod_dar_1)
#Model 2
summary(logitMod_dar_2)

#confusion matrix & error
InformationValue::confusionMatrix(testDar$RainTomorrow, dar_predicted)
InformationValue::misClassError(testDar$RainTomorrow, dar_predicted)

#ROC Curve
roc(testDar$RainTomorrow, dar_predicted, plot =TRUE, legacy.axes = TRUE, percent = TRUE, 
    main="ROC Curve for Darwin  Prediction Model",
    xlab="False Positive Percentage", 
    ylab="True Positive Percentage", lwd=4, print.auc = TRUE)
par(pty="m")

####################################
############Hobart area#############
####################################
source("hobart.R")
#Model 1
summary(logitMod_hobart_1)
#Model 2
summary(logitMod_hobart_2)

#confusion matrix & error
InformationValue::confusionMatrix(testHobart$RainTomorrow, hobart_predicted)
InformationValue::misClassError(testHobart$RainTomorrow, hobart_predicted)

#ROC Curve
roc(testHobart$RainTomorrow, hobart_predicted, plot =TRUE, legacy.axes = TRUE, percent = TRUE, 
    main="ROC Curve for Hobart  Prediction Model",
    xlab="False Positive Percentage", 
    ylab="True Positive Percentage", lwd=4, print.auc = TRUE)
par(pty="m")

####################################
##########Melbourne area############
####################################
source("melbourne.R")
#Model 1
summary(logitMod_mel_1)
#Model 2
summary(logitMod_mel_2)

#confusion matrix & error
InformationValue::confusionMatrix(testMel$RainTomorrow, mel_predicted)
InformationValue::misClassError(testMel$RainTomorrow, mel_predicted)

roc(testMel$RainTomorrow, mel_predicted, plot =TRUE, legacy.axes = TRUE, percent = TRUE, 
    main="ROC Curve for Melbourne  Prediction Model",
    xlab="False Positive Percentage", 
    ylab="True Positive Percentage", lwd=4, print.auc = TRUE)
par(pty="m")
####################################
############Perth area##############
####################################
source("perth.R")

#Model 1
summary(logitMod_P_1)
#Model 2
summary(logitMod_P_2)

#confusion matrix & error
InformationValue::confusionMatrix(testP$RainTomorrow, P_predicted)
InformationValue::misClassError(testP$RainTomorrow, P_predicted)

roc(testP$RainTomorrow, P_predicted, plot =TRUE, legacy.axes = TRUE, percent = TRUE, 
    main="ROC Curve for Perth  Prediction Model",
    xlab="False Positive Percentage", 
    ylab="True Positive Percentage", lwd=4, print.auc = TRUE)
par(pty="m")
####################################
############Sydney area#############
####################################
source("sydney.R")

#Model 1
summary(logitMod_syd_1)
#Model 2
summary(logitMod_syd_2)
#Model 3
summary(logitMod_syd_3)

#confusion matrix & error
InformationValue::confusionMatrix(testSyd$RainTomorrow, syd_predicted)
InformationValue::misClassError(testSyd$RainTomorrow, syd_predicted)

roc(testSyd$RainTomorrow, syd_predicted, plot =TRUE, legacy.axes = TRUE, percent = TRUE, 
    main="ROC Curve for Sydney  Prediction Model",
    xlab="False Positive Percentage", 
    ylab="True Positive Percentage", lwd=4, print.auc = TRUE)
par(pty="m")