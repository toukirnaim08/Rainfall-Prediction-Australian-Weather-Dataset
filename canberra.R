####################################
##########Canberra area############
####################################
can_df<-final_df[which(final_df$Location=="Canberra"),]

###traing & test data
can_smp_size <- floor(0.75 * nrow(can_df))
set.seed(123)
can_train_ind <- sample(seq_len(nrow(can_df)), size = can_smp_size)
trainCan <- can_df[can_train_ind, ]
testCan <- can_df[-can_train_ind, ]

#logistic Regression

#model
can_1 <- glm(RainTomorrow ~ Evaporation + Sunshine + WindGustDir + 
                        WindGustSpeed + WindDir9am + WindDir3pm + 
                        WindSpeed9am + WindSpeed3pm + Humidity9am + 
                        Humidity3pm + Cloud9am + Cloud3pm + 
                        RainToday + DailyTempMean + DailyPressMean, 
                      data=trainCan, family=binomial(link="logit"))

#stepwise formula to find out best model
backwards = step(can_1,trace=0)
formula(backwards)

can_2 <- glm(RainTomorrow ~ Sunshine + WindGustSpeed + WindSpeed9am + 
                        WindSpeed3pm + Humidity3pm + Cloud3pm + 
                        RainToday + DailyTempMean + DailyPressMean, 
                      data=trainCan, family=binomial(link="logit"))
summary(can_2)

can_3 <- glm(RainTomorrow ~ Sunshine + WindGustSpeed + WindSpeed9am + 
               WindSpeed3pm + Humidity3pm + Cloud3pm + 
               DailyTempMean + DailyPressMean, 
                      data=trainCan, family=binomial(link="logit"))
summary(can_3)
#prediction
options(scipen=999)
can_predicted <- predict(can_3, testCan, type="response")


###########################
#Model Without outliers
############################
can_no_outliers<-can_df%>%filter(can_df$Evaporation<15)
can_no_outliers<-can_no_outliers%>%filter(can_no_outliers$DailyPressMean>993)

###traing & test data
can_no_outlier_smp <- floor(0.75 * nrow(can_no_outliers))
set.seed(123)
can_train_no_outlier <- sample(seq_len(nrow(can_no_outliers)), size = can_no_outlier_smp)
trainCan_no_outlier <- can_no_outliers[can_train_no_outlier, ]
#logistic Regression

#model
can_no_outlier_1 <- glm(RainTomorrow ~ Evaporation + Sunshine + 
                          WindGustDir + WindGustSpeed +
                          WindDir9am + WindDir3pm + WindSpeed9am  +
                          WindSpeed3pm+ Humidity9am+
                          Humidity3pm +
                          RainToday + DailyTempMean + DailyPressMean, 
                        data=trainCan_no_outlier, family=binomial(link="logit"))

#stepwise formula to find out best model
backwards = step(can_no_outlier_1,trace=0)
formula(backwards)

can_no_outlier_2 <- glm(RainTomorrow ~ Evaporation + Sunshine + WindGustSpeed + WindSpeed9am + 
                          WindSpeed3pm + Humidity3pm + DailyTempMean + DailyPressMean, 
                        data=trainCan_no_outlier, family=binomial(link="logit"))



#prediction
options(scipen=999)
can_predicted_no_outlier<-predict(can_no_outlier_2, testCan, type="response")
can.predicted.df_no_outlier<-testCan%>%mutate(can_predicted_no_outlier = can_predicted_no_outlier)
can.predicted.df_no_outlier<-can.predicted.df_no_outlier%>%mutate(can_predicted_no_outlier=ifelse(can_predicted_no_outlier > 0.5, 1, 0))




