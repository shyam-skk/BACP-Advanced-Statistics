getwd()
mydata<-read.csv("PL_X_SELL.csv",header = TRUE)
dim(mydata)
names(mydata)
str(mydata)

head(mydata)
tail(mydata)

colSums(is.na(mydata)) 


########## descriptive analysis ################
summary(mydata)
attach(mydata)

range(Age)
range(Balance)
range(No_OF_CR_TXNS)
range(SCR)
range(Holding_Period)

sd(Age)
sd(Balance)
sd(No_OF_CR_TXNS)
sd(SCR)
sd(Holding_Period)

var(Age)
var(Balance)
var(No_OF_CR_TXNS)
var(SCR)
var(Holding_Period)

table(Target)
table(Gender)
table(Occupation)
table(AGE_BKT)

table(Gender,Occupation)


################ plotes

library(lattice)

plot(density(Balance),col='green')
hist(Balance,main = 'Avg Balance',xlab = 'Balance',ylab = 'Frequency',col = 'orange')



plot(Occupation,Balance,main = 'Occupation vs Balance',xlab = 'Occupation',ylab = 'balance',col = 'green')
plot(Gender,Balance,main = 'Gender vs Balance',xlab = 'gender',ylab = 'balance',col = 'pink')


par(mfrow=c(1,2)) 
boxplot(Balance,main = 'Avg Balance',xlab = 'Balance',ylab = 'Frequency',col = 'yellow')
plot(Occupation,main = 'Occupation',xlab = 'occupation',ylab = 'Frequency',col = 'green')
plot(Gender,main = 'gender',xlab = 'gender',ylab = 'Frequency',col = 'pink')
plot(Occupation,Gender,main = 'Occupation Vs Gender',xlab = 'Occupation',ylab = 'Gender')

dev.off()

install.packages('dplyr')
library(dplyr)

levels(Occupation)

group_by(mydata, Occupation) %>%
  summarise(
    count = n(),
    mean = mean(Balance, na.rm = TRUE),
    sd = sd(Balance, na.rm = TRUE)
  )
group_by(mydata, Gender) %>%
  summarise(
    count = n(),
    mean = mean(Balance, na.rm = TRUE),
    sd = sd(Balance, na.rm = TRUE)
  )
