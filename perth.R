####################################
############Perth area##############
####################################
perth_df<-final_df[final_df$Location %in% c("Perth", "PerthAirport"), ]

###traing & test data
p_smp_size <- floor(0.75 * nrow(perth_df[,3:columnSize]))
set.seed(123)
p_train_ind <- sample(seq_len(nrow(perth_df[,3:columnSize])), size = p_smp_size)
trainP <- perth_df[,3:columnSize][p_train_ind, ]
testP <- perth_df[,3:columnSize][-p_train_ind, ]

#logistic Regression

#model
logitMod_P_1 <- glm(RainTomorrow ~ Evaporation + Sunshine + WindGustDir + 
                      WindGustSpeed + WindDir9am + WindDir3pm + 
                      WindSpeed9am  + WindSpeed3pm + Humidity9am + 
                      Humidity3pm + RainToday + DailyTempMean + 
                      DailyPressMean, 
                    data=trainP, family=binomial(link="logit"))



#stepwise formula to find out best model
backwards = step(logitMod_P_1,trace=0)
formula(backwards)

logitMod_P_2 <- glm(RainTomorrow ~ Evaporation + Sunshine + WindGustSpeed + 
                      WindDir9am + WindDir3pm + WindSpeed9am + 
                      WindSpeed3pm + Humidity3pm + DailyTempMean + 
                      DailyPressMean, 
                    data=trainP, family=binomial(link="logit"))



#prediction
options(scipen=999)
P_predicted <- predict(logitMod_P_2, testP, type="response")

