library(lmtest)
library(olsrr)

mod_1 <- lm(percentage ~ Sex, data = psycho_data)
summary(mod_1)

mod_2 <- lm(percentage ~ GHQ, data = psycho_data)
summary(mod_2)

mod_3 <- lm(percentage ~ Age, data = psycho_data)
summary(mod_3)

mod_3 <- lm(percentage ~ MeanAge, data = psycho_data)
summary(mod_3)

mod_4 <- lm(percentage ~ Sex + GHQ, data = psycho_data)
summary(mod_4)

mod_5 <- lm(percentage ~ Sex + Age, data = psycho_data)
summary(mod_5)

mod_6 <- lm(percentage ~ Age + GHQ, data = psycho_data)
summary(mod_6)

mod_7 <- lm(percentage ~ Sex + GHQ + Age, data = psycho_data)
summary(mod_7)

mod_8 <- lm(transformed ~ Sex, data = psycho_data)
summary(mod_8)

mod_9 <- lm(transformed ~ GHQ, data = psycho_data)
summary(mod_9)

mod_10 <- lm(transformed ~ Age, data = psycho_data)
summary(mod_10)

mod_11 <- lm(transformed ~ Sex + GHQ, data = psycho_data)
summary(mod_11)

mod_12 <- lm(transformed ~ Sex + Age, data = psycho_data)
summary(mod_12)

mod_13 <- lm(transformed ~ Age + GHQ, data = psycho_data)
summary(mod_13)

mod_14 <- lm(transformed ~ Sex + GHQ + Age, data = psycho_data)
summary(mod_14)

mod_15 <- lm(transformed ~ MeanAge, data = psycho_data)
summary(mod_15)

mod_16 <- lm(transformed ~ Sex + MeanAge, data = psycho_data)
summary(mod_16)

mod_17 <- lm(transformed ~ MeanAge + GHQ, data = psycho_data)
summary(mod_17)

mod_18 <- lm(transformed ~ Sex + GHQ + MeanAge, data = psycho_data)
summary(mod_18)

mod_19 <- lm(transformed ~ (Sex*Age) + GHQ, data = psycho_data)
summary(mod_19)

mod_20 <- lm(transformed ~ Sex + (GHQ*Age), data = psycho_data)
summary(mod_20)

mod_21 <- lm(transformed ~ (Sex*GHQ) + Age, data = psycho_data)
summary(mod_21)

mod_22 <- lm(transformed ~ (Sex*MeanAge) + GHQ, data = psycho_data)
summary(mod_22)

mod_23 <- lm(transformed ~ Sex + (GHQ*MeanAge), data = psycho_data)
summary(mod_23)

mod_24 <- lm(transformed ~ (Sex*GHQ) + MeanAge, data = psycho_data)
summary(mod_24)


anova(mod_14)
anova(mod_19)
anova(mod_20)
anova(mod_21)

par(mfrow=c(2,2))

#Check Linearity

plot(summary(mod_14)$residuals, main="Scatter Plot of Residuals mod_14")
abline(h=0.02, lty=2)

plot(summary(mod_19)$residuals, main="Scatter Plot of Residuals mod_19")
abline(h=0.02, lty=2)

plot(summary(mod_20)$residuals, main="Scatter Plot of Residuals mod_20")
abline(h=0.02, lty=2)

plot(summary(mod_21)$residuals, main="Scatter Plot of Residuals mod_21")
abline(h=0.02, lty=2)


#Check Normality
qqnorm(residuals(mod_7), main="Normal Plot of Residuals mod_7")
qqnorm(residuals(mod_14), main="Normal Plot of Residuals mod_14")
qqnorm(residuals(mod_19), main="Normal Plot of Residuals mod_19")
qqnorm(residuals(mod_20), main="Normal Plot of Residuals mod_20")
qqnorm(residuals(mod_21), main="Normal Plot of Residuals mod_21")

#Check Constant Variance
plot(fitted.values(mod_14),residuals(mod_14))
abline(h=0, lty=3)
plot(fitted.values(mod_19),residuals(mod_19))
abline(h=0, lty=3)
plot(fitted.values(mod_20),residuals(mod_20))
abline(h=0, lty=3)
plot(fitted.values(mod_21),residuals(mod_21))
abline(h=0, lty=3)

#Check outliers and influential observations
glinf = influence(mod_14)
g1s = summary(mod_14)
studres=residuals(mod_14)/(g1s$sigma*(1-glinf$hat))
plot(studres, main="Studentized Residual Plot mod_14",ylab="Studentized Residuals",xlab="",pch=16)
abline(h=0, lty=3)
abline(h=2, lty=3)
abline(h=-2, lty=3)

glinf = influence(mod_19)
g1s = summary(mod_19)
studres=residuals(mod_21)/(g1s$sigma*(1-glinf$hat))
plot(studres, main="Studentized Residual Plot mod_19",ylab="Studentized Residuals",xlab="",pch=16)
abline(h=0, lty=3)
abline(h=2, lty=3)
abline(h=-2, lty=3)

glinf = influence(mod_20)
g1s = summary(mod_20)
studres=residuals(mod_20)/(g1s$sigma*(1-glinf$hat))
plot(studres, main="Studentized Residual Plot mod_20",ylab="Studentized Residuals",xlab="",pch=16)
abline(h=0, lty=3)
abline(h=2, lty=3)
abline(h=-2, lty=3)

glinf = influence(mod_21)
g1s = summary(mod_21)
studres=residuals(mod_21)/(g1s$sigma*(1-glinf$hat))
plot(studres, main="Studentized Residual Plot mod_21",ylab="Studentized Residuals",xlab="",pch=16)
abline(h=0, lty=3)
abline(h=2, lty=3)
abline(h=-2, lty=3)



#Extra
plot(psycho_data$Sex,summary(mod_14)$residuals)
plot(psycho_data$Sex,summary(mod_19)$residuals)
plot(psycho_data$Sex,summary(mod_20)$residuals)
plot(psycho_data$Sex,summary(mod_21)$residuals)







