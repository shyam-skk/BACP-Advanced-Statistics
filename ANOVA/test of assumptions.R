getwd()
mydata<-read.csv("PL_X_SELL.csv",header = TRUE)

attach(mydata)

library(psych)
install.packages("car")
library(car)
library(foreign)
install.packages("MASS")
library(MASS)

for (i in unique(factor(mydata$Occupation))){   cat(shapiro.test(mydata[mydata$Occupation==i, ]$Balance)$p.value," ") }

ks.test(mydata$Occupation,mydata$Balance)




levels(Occupation)


newdata1<- subset(mydata, Occupation == 'PROF', select=c(Balance))
newdata2<- subset(mydata, Occupation == 'SAL', select=c(Balance))
newdata3<- subset(mydata, Occupation == 'SELF-EMP', select=c(Balance))
newdata4<- subset(mydata, Occupation == 'SENP', select=c(Balance))

View(newdata1)
View(newdata2)
View(newdata3)
View(newdata4)

library(nortest)
ad.test(newdata1$Balance)$p.value
ad.test(newdata2$Balance)$p.value
ad.test(newdata3$Balance)$p.value
ad.test(newdata4$Balance)$p.value

m <- mean(newdata1$Balance) 
s <- sd(newdata1$Balance) 
ks.test(newdata1$Balance, "pnorm", m, s)

m <- mean(newdata2$Balance) 
s <- sd(newdata2$Balance) 
ks.test(newdata2$Balance, "pnorm", m, s)

m <- mean(newdata3$Balance) 
s <- sd(newdata3$Balance) 
ks.test(newdata3$Balance, "pnorm", m, s)


##############  test of variance
leveneTest(Balance~Occupation)
bartlett.test(Balance~Occupation)


############# anova 
aov1<-aov(Balance~Occupation)
summary(aov1)

######## robust
oneway.test(Balance~Occupation,var.equal = FALSE)

model1<-lm(Balance~Occupation)
Anova(model1,type = "II",white.adjust = TRUE)

######## post hoc
TukeyHSD(aov1)

plot(TukeyHSD(aov1))
plot(aov1,1)
plot(aov1,2)
