df <- data.frame(PastFrequency=c(0.6, 0.3, 0.4, 0.4, 0.2, 0.6, 0.3, 0.4, 0.9, 0.2),
                 BloodPressure=c(103, 87, 32, 42, 59, 109, 78, 205, 135, 176),
                 FirstDecision=c(1, 1, 1, 1, 0, 0, 0, 0, NA, 1),
                 SecondDecision=c(0, 0, 1, 1, 0, 0, 1, 1, 1, 1),
                 FinalDecision=c(0, 1, 0, 1, 0, 1, 0, 1, 1, 1))
mapply(mean,df[,1:5],na.rm=TRUE)
mapply(median,df[,1:5],na.rm=TRUE)

par(mfrow=c(1,2))

hist(df$PastFrequency, main='Distribution of Hospital Visits in the Past 12 months', xlab='12-month Visits', ylab='Frequency')
text(x=0.43,y=4,'mean',col='red')
abline(v=mean(df$PastFrequency),col='red')

hist(df$BloodPressure, main='Distribution of Blood Pressure (mm Hg)', xlab='Blood Pressure (mm Hg)', ylab='Frequency')
text(x=102.6,y=3,'mean',col='red')
abline(v=mean(df$BloodPressure), col='red')

par(mfrow=c(1,3))

hist(df$FirstDecision, main='Distribution of First Assessment', xlab='First Assessment by General Doctor (Bad=1, Good=0)', ylab='Score')
text(x=0.5555556,y=5,'mean',col='red')
abline(v=mean(df$FirstDecision,na.rm=TRUE), col='red')

hist(df$SecondDecision, main='Distribution of Second Assessment', xlab='Second Assessment by External Doctor (Bad=1, Good=0)', ylab='Score')
text(x=0.6,y=6,'mean',col='red')
abline(v=mean(df$SecondDecision), col='red')

hist(df$FinalDecision, main='Distribution of Emergency Department Assessment', xlab='Likelihood of Getting Immediate Care (Low=0, High=1)', ylab='Score')
text(x=0.6,y=6,'mean',col='red')
abline(v=mean(df$FinalDecision), col='red')

boxplot(df$BloodPressure~df$FirstDecision, main='Assessment of Blood Pressure by General Doctor', ylab='Blood Pressure', xlab='General Doctor Assessment', names=c('Good','Bad'))
boxplot(df$BloodPressure~df$SecondDecision, main='Assessment of Blood Pressure by External Doctor', ylab='Blood Pressure', xlab='External Doctor Assessment', names=c('Good','Bad'))
boxplot(df$BloodPressure~df$FinalDecision, main='Assessment of Blood Pressure by the Emergency Department', ylab='Blood Pressure', xlab='Immediate Care Likelihood', names=c('Low','High'))
