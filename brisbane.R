####################################
###########Brisbane area############
####################################
bris_df<-final_df[which(final_df$Location=="Brisbane"),]

columnSize<-18
###traing & test data
bris_smp_size <- floor(0.75 * nrow(bris_df[,3:columnSize]))
set.seed(123)
bris_train_ind <- sample(seq_len(nrow(bris_df[,3:columnSize])), size = bris_smp_size)
trainBris <- bris_df[,3:columnSize][bris_train_ind, ]
testBris <- bris_df[,3:columnSize][-bris_train_ind, ]

#logistic Regression

#model
logitMod_bris_1 <- glm(RainTomorrow ~ Evaporation + Sunshine + WindGustDir + 
                         WindGustSpeed + WindDir9am + WindDir3pm + 
                         WindSpeed9am + WindSpeed3pm + Humidity9am + 
                         Humidity3pm + RainToday + DailyTempMean + 
                         DailyPressMean, 
                      data=trainBris, family=binomial(link="logit"))



#stepwise formula to find out best model
backwards = step(logitMod_bris_1,trace=0)
formula(backwards)

logitMod_bris_2 <- glm(RainTomorrow ~ Evaporation + Sunshine + WindGustDir + 
                         WindGustSpeed + WindDir3pm + WindSpeed9am + 
                         Humidity9am + Humidity3pm, 
                      data=trainBris, family=binomial(link="logit"))



logitMod_bris_3 <- glm(RainTomorrow ~ Evaporation + Sunshine + WindGustSpeed + 
                         WindDir3pm + Humidity9am + Humidity3pm, 
                      data=trainBris, family=binomial(link="logit"))



#prediction
options(scipen=999)
bris_predicted <- predict(logitMod_bris_3, testBris, type="response")
