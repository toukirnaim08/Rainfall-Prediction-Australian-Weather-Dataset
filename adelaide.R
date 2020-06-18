####################################
##########Adelaide area#############
####################################
adl_df<-final_df[which(final_df$Location=="Adelaide"),]
adl_df_no_cloud<-adl_df%>%select(-c(Cloud9am,Cloud3pm))


#Model With outliers
###traing & test data
adl_smp_size <- floor(0.75 * nrow(adl_df_no_cloud))
set.seed(123)
train_ind <- sample(seq_len(nrow(adl_df_no_cloud)), size = adl_smp_size)
trainAdl <- adl_df_no_cloud[train_ind, ]
testAdl <- adl_df_no_cloud[-train_ind, ]
#logistic Regression

#model
adl_1 <- glm(RainTomorrow ~ Evaporation + Sunshine + 
                        WindGustDir + WindGustSpeed +
                        WindDir9am + WindDir3pm + WindSpeed9am  +
                        WindSpeed3pm+ Humidity9am+
                        Humidity3pm +
                        RainToday + DailyTempMean + DailyPressMean, 
                      data=trainAdl, family=binomial(link="logit"))

#stepwise formula to find out best model
backwards = step(adl_1,trace=0)
formula(backwards)

adl_2 <- glm(RainTomorrow ~ Sunshine + WindGustSpeed + WindDir9am + WindDir3pm + 
                        WindSpeed3pm + Humidity3pm + DailyTempMean + DailyPressMean, 
                      data=trainAdl, family=binomial(link="logit"))

summary(adl_2)
adl_3 <- glm(RainTomorrow ~ Sunshine + WindGustSpeed + WindDir9am + 
                        WindSpeed3pm + Humidity3pm + DailyTempMean + DailyPressMean, 
                      data=trainAdl, family=binomial(link="logit"))

summary(adl_3)

#prediction
options(scipen=999)
adl_predicted <- predict(adl_3, testAdl, type="response")
predicted.df<-testAdl%>%mutate(pred = adl_predicted)
predicted.df <- predicted.df%>%mutate(pred=ifelse(adl_predicted > 0.5, 1, 0))

###########################
#Model Without outliers
############################
adl_no_outliers<-adl_df_no_cloud%>%filter(adl_df_no_cloud$Evaporation<30)
adl_no_outliers<-adl_no_outliers%>%filter(adl_no_outliers$DailyPressMean>990)

###traing & test data
adl_no_outlier_smp <- floor(0.75 * nrow(adl_no_outliers))
set.seed(123)
adl_train_no_outlier <- sample(seq_len(nrow(adl_no_outliers)), size = adl_no_outlier_smp)
trainAdl_no_outlier <- adl_no_outliers[adl_train_no_outlier, ]
testAdl_no_outlier <- adl_no_outliers[-adl_train_no_outlier, ]
#logistic Regression

#model
adl_no_outlier_1 <- glm(RainTomorrow ~ Evaporation + Sunshine + 
                                   WindGustDir + WindGustSpeed +
                                   WindDir9am + WindDir3pm + WindSpeed9am  +
                                   WindSpeed3pm+ Humidity9am+
                                   Humidity3pm +
                                   RainToday + DailyTempMean + DailyPressMean, 
                      data=trainAdl_no_outlier, family=binomial(link="logit"))

#stepwise formula to find out best model
backwards = step(adl_no_outlier_1,trace=0)
formula(backwards)

adl_no_outlier_2 <- glm(RainTomorrow ~ Sunshine + WindGustSpeed + WindDir9am + WindDir3pm + 
                                   WindSpeed3pm + Humidity3pm + DailyTempMean + DailyPressMean, 
                      data=trainAdl_no_outlier, family=binomial(link="logit"))

summary(adl_no_outlier_2)

adl_no_outlier_3 <- glm(RainTomorrow ~ Sunshine + WindGustSpeed + WindDir9am + 
                                   WindSpeed3pm + Humidity3pm + DailyTempMean + DailyPressMean, 
                      data=trainAdl_no_outlier, family=binomial(link="logit"))

summary(adl_no_outlier_3)

#prediction
options(scipen=999)
adl_predicted_no_outlier<-predict(adl_no_outlier_3, testAdl, type="response")
predicted.df_no_outlier<-testAdl%>%mutate(pred = adl_predicted_no_outlier)
predicted.df_no_outlier<-predicted.df_no_outlier%>%mutate(pred=ifelse(adl_predicted_no_outlier > 0.5, 1, 0))



