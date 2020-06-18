####################################
############Melbourne area############
####################################
mel_df<-final_df[final_df$Location %in% c("Melbourne", "MelbourneAirport"), ]

###traing & test data
mel_smp_size <- floor(0.75 * nrow(mel_df[,3:columnSize]))
set.seed(123)
mel_train_ind <- sample(seq_len(nrow(mel_df[,3:columnSize])), size = mel_smp_size)
trainMel <- mel_df[,3:columnSize][mel_train_ind, ]
testMel <- mel_df[,3:columnSize][-mel_train_ind, ]

#logistic Regression

#model
logitMod_mel_1 <- glm(RainTomorrow ~ Evaporation + Sunshine + 
                        WindGustDir + WindGustSpeed +
                        WindDir9am + WindDir3pm + WindSpeed9am  +
                        WindSpeed3pm+ Humidity9am+
                        Humidity3pm +
                        RainToday + DailyTempMean + DailyPressMean, 
                      data=trainMel, family=binomial(link="logit"))



#stepwise formula to find out best model
backwards = step(logitMod_mel_1,trace=0)
formula(backwards)

logitMod_mel_2 <- glm(RainTomorrow ~ Evaporation + Sunshine + WindGustSpeed + WindDir9am + 
                        WindSpeed9am + WindSpeed3pm + Humidity3pm + RainToday + DailyTempMean + 
                        DailyPressMean, 
                      data=trainMel, family=binomial(link="logit"))



#prediction
options(scipen=999)
mel_predicted <- predict(logitMod_mel_2, testMel, type="response")


