####################################
############Darwin area#############
####################################
dar_df<-final_df[which(final_df$Location=="Darwin"),]

###traing & test data
dar_smp_size <- floor(0.75 * nrow(dar_df[,3:columnSize]))
set.seed(123)
dar_train_ind <- sample(seq_len(nrow(dar_df[,3:columnSize])), size = dar_smp_size)
trainDar <- dar_df[,3:columnSize][dar_train_ind, ]
testDar <- dar_df[,3:columnSize][-dar_train_ind, ]

#logistic Regression
#model
logitMod_dar_1 <- glm(RainTomorrow ~ Evaporation + Sunshine + WindGustDir + 
                        WindGustSpeed + WindDir9am + WindDir3pm + 
                        WindSpeed9am  + WindSpeed3pm + Humidity9am + 
                        Humidity3pm + RainToday + DailyTempMean + 
                        DailyPressMean, 
                      data=trainDar, family=binomial(link="logit"))


#stepwise formula to find out best model
backwards = step(logitMod_dar_1,trace=0)
formula(backwards)

logitMod_dar_2 <- glm(RainTomorrow ~ Sunshine + WindGustSpeed + WindDir3pm + 
                        WindSpeed9am + WindSpeed3pm + Humidity3pm + 
                        RainToday + DailyTempMean + DailyPressMean, 
                      data=trainDar, family=binomial(link="logit"))


#prediction
options(scipen=999)
dar_predicted <- predict(logitMod_dar_2, testDar, type="response")

