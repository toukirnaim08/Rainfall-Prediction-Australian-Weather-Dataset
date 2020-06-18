####################################
############Sydney area############
####################################
syd_df<-final_df[which(final_df$Location=="Sydney" | final_df$Location == "SydneyAirport"),]

###traing & test data
syd_smp_size <- floor(0.75 * nrow(syd_df[,3:columnSize]))
set.seed(123)
syd_train_ind <- sample(seq_len(nrow(syd_df[,3:columnSize])), size = syd_smp_size)
trainSyd <- syd_df[,3:columnSize][syd_train_ind, ]
testSyd <- syd_df[,3:columnSize][-syd_train_ind, ]

#logistic Regression

#model
logitMod_syd_1 <- glm(RainTomorrow ~ Evaporation + Sunshine + WindGustDir + 
                        WindGustSpeed + WindDir9am + WindDir3pm + 
                        WindSpeed9am  + WindSpeed3pm + Humidity9am + 
                        Humidity3pm + RainToday + DailyTempMean + 
                        DailyPressMean, 
                    data=trainSyd, family=binomial(link="logit"))



#stepwise formula to find out best model
backwards = step(logitMod_syd_1,trace=0)
formula(backwards)

logitMod_syd_2 <- glm(RainTomorrow ~ Sunshine + WindGustSpeed + WindDir3pm + 
                        WindSpeed9am + Humidity9am + Humidity3pm + 
                        RainToday + DailyTempMean + DailyPressMean, 
                    data=trainSyd, family=binomial(link="logit"))


logitMod_syd_3 <- glm(RainTomorrow ~ Sunshine + WindGustSpeed + Humidity3pm + 
                        RainToday + DailyPressMean, 
                    data=trainSyd, family=binomial(link="logit"))



#prediction
options(scipen=999)
syd_predicted <- predict(logitMod_syd_3, testSyd, type="response")
