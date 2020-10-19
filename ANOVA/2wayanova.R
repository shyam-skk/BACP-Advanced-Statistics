getwd()
mydata<-read.csv("PL_X_SELL.csv",header = TRUE)

attach(mydata)

install.packages("robustHD")
install.packages("rcompanion")
install.packages("WRS2")

library(robustHD)
library(rcompanion)
library(WRS2)
library(psych)
library(car)
library(foreign)
library(MASS)


Occupation<-factor(Occupation,labels = c("PROF","SAL","SELF-EMP","SENP"))
Gender<-factor(Gender,labels = c("F","M","O"))

tapply(Balance,list(Occupation,Gender),mean)
tapply(Balance,list(Occupation,Gender),sd)

interaction.plot(Gender,Occupation,Balance)

ks.test(mydata$Occupation,mydata$Balance)
ks.test(mydata$Gender,mydata$Balance)


leveneTest(Balance~Occupation*Gender)


aov2<- aov(Balance~Occupation+Gender+Occupation:Gender, data=mydata) 
summary(aov2)



pbad<-pbad2way(Balance ~ Occupation + Gender + Occupation:Gender, 
                 data = mydata)

model2<-lm(Balance~Occupation + Gender + Occupation:Gender)
Anova(model2,type = "II",white.adjust = TRUE)

TukeyHSD(aov2, which = "dose")
TukeyHSD(aov2)
