setwd("C:/Users/SHYAM KRISHNAN K/Desktop/PJ-3/pca")
getwd()

bat<-read.csv("batting_bowling_ipl_bat.csv",header = TRUE)
colSums(is.na(bat))
dim(bat)

bat<- na.omit(bat)
dim(bat)
colSums(is.na(bat))

names(bat)
str(bat)
head(bat)
tail(bat)

mydata1<-bat[2:7] 
str(mydata1)
names(mydata1)

############### descriptive data analysis
summary(mydata1)
attach(mydata1)

range(Runs)
range(Ave)
range(SR)
range(Fours)
range(Sixes)
range(HF)

sd(Runs)
sd(Ave)
sd(SR)
sd(Fours)
sd(Sixes)
sd(HF)

var(Runs)
var(Ave)
var(SR)
var(Fours)
var(Sixes)
var(HF)

######### visualization 
plot(mydata1, pch=1, col="blue", main="Scatterplot of complete variables")

par(mfrow=c(3,2))
hist(Runs,main = 'Runs',xlab = 'runs',ylab = 'Frequency',col = 'green')
hist(Ave,main = 'Averege Score',xlab = 'avg',ylab = 'Frequency',col = 'pink')
hist(SR,main = 'Strike Rate',xlab = 'strike rate',ylab = 'Frequency',col = 'brown')
hist(Fours,main = 'Fours',xlab = 'fours',ylab = 'Frequency',col = 'blue')
hist(Sixes,main = 'Sixes',xlab = 'sixes',ylab = 'Frequency',col = 'violet')
hist(HF,main = 'Half century',xlab = 'HF',ylab = 'Frequency',col = 'yellow')

boxplot(Runs,main = 'Runs',xlab = 'runs',ylab = 'Frequency',col = 'green')
boxplot(Ave,main = 'Averege Score',xlab = 'avg',ylab = 'Frequency',col = 'pink')
boxplot(SR,main = 'Strike Rate',xlab = 'strike rate',ylab = 'Frequency',col = 'brown')
boxplot(Fours,main = 'Fours',xlab = 'fours',ylab = 'Frequency',col = 'blue')
boxplot(Sixes,main = 'Sixes',xlab = 'sixes',ylab = 'Frequency',col = 'violet')
boxplot(HF,main = 'Half century',xlab = 'HF',ylab = 'Frequency',col = 'yellow')

dev.off()
######### PRINCIPAL COMPONENT ANALYSIS ##############
install.packages('GPArotation')
install.packages('ggfortify')
install.packages('nFactors')
install.packages('Hmisc')

library(corpcor)
library(GPArotation)
library(psych)
library(ggplot2) 
library(ggfortify)
library(nFactors) 
library(dplyr) 
library(expm)
library(Hmisc) 

########### Correlation matrix 
corrmatrix1<-cor(mydata1)
corrmatrix1 


############# Bartlett Sphericity Test 
print(cortest.bartlett(corrmatrix1, nrow(mydata1))) 


#############Running PCA using basic commands
A<-eigen(corrmatrix1)
eigenvectors1<-A$vectors
eigenvalues1<-A$values
eigenvectors1
eigenvalues1

D<-diag(eigenvalues1)
D

### principal componenet loadings
pc1<-principal(mydata1,nfactors = length(mydata1),rotate="none")
pc1

# Interpreting the variance
part.pca<-eigenvalues1/sum(eigenvalues1)*100
part.pca

######## Screeplots 
plot(eigenvalues1,type="lines",xlab="Pincipal Components",ylab="Eigen Values")
ee<-sqrt(eigenvalues1)
pcaloading <-eigenvectors1*ee
pcaloading

# Principal Components Scoring and Perceptual Map
RKPsc<-scale(mydata1)
z<-as.matrix(RKPsc%*%eigenvectors1)
z
pc.cr<-princomp(RKPsc,cor=TRUE)
summary(pc.cr)
# Check for Cumulative Proportion
plot(pc.cr)
biplot(pc.cr)


matrixxx<-data.matrix(mydata1)
pr.out = prcomp(matrixxx, scale=TRUE)
pr.var = pr.out$sdev^2
pve = pr.var/sum(pr.var)
pr.out$x
scores = as.data.frame(pr.out$x)



