####################################
############Hobart area#############
####################################
hobart_df<-final_df[which(final_df$Location=="Hobart"),]

###traing & test data
hobart_smp_size <- floor(0.75 * nrow(hobart_df[,3:columnSize]))
set.seed(123)
hobart_train_ind <- sample(seq_len(nrow(hobart_df[,3:columnSize])), size = hobart_smp_size)
trainHobart <- hobart_df[,3:columnSize][hobart_train_ind, ]
testHobart <- hobart_df[,3:columnSize][-hobart_train_ind, ]

#logistic Regression

#model
logitMod_hobart_1 <- glm(RainTomorrow ~ Evaporation + Sunshine + WindGustDir + 
                           WindGustSpeed + WindDir9am + WindDir3pm + 
                           WindSpeed9am  + WindSpeed3pm + Humidity9am + 
                           Humidity3pm + RainToday + DailyTempMean + 
                           DailyPressMean, 
                      data=trainHobart, family=binomial(link="logit"))



#stepwise formula to find out best model
backwards = step(logitMod_hobart_1,trace=0)
formula(backwards)

logitMod_hobart_2 <- glm(RainTomorrow ~ Sunshine + WindGustSpeed + WindSpeed3pm + 
                           Humidity3pm + DailyPressMean, 
                      data=trainHobart, family=binomial(link="logit"))


#prediction
options(scipen=999)
hobart_predicted <- predict(logitMod_hobart_2, testHobart, type="response")

