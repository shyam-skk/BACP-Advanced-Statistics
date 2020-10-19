setwd("C:/Users/SHYAM KRISHNAN K/Desktop/PJ-3/pca")
getwd()

bowl<-read.csv("batting_bowling_ipl_bowl.csv",header = TRUE)
colSums(is.na(bowl))
dim(bowl)

bowl<- na.omit(bowl)
dim(bowl)
colSums(is.na(bowl))

names(bowl)
str(bowl)
head(bowl)
tail(bowl)

mydata2<-bowl[2:5] 
str(mydata2)
names(mydata2)

############### descriptive data analysis
summary(mydata2)
attach(mydata2)

range(Wkts)
range(Ave)
range(Econ)
range(SR)

sd(Wkts)
sd(Ave)
sd(Econ)
sd(SR)

var(Wkts)
var(Ave)
var(Econ)
var(SR)

######### visualization 
plot(mydata2, pch=1, col="red", main="Scatterplot of complete variables")

par(mfrow=c(2,2))
hist(Wkts,main = 'Wickets ',xlab = 'Wickets ',ylab = 'Frequency',col = 'green')
hist(Ave,main = 'Bowling Average ',xlab = 'average ',ylab = 'Frequency',col = 'pink')
hist(Econ,main = 'Economy Rate',xlab = 'economy',ylab = 'Frequency',col = 'brown')
hist(SR,main = 'Strike Rate ',xlab = 'strike Rate ',ylab = 'Frequency',col = 'blue')
dev.off()

boxplot(Wkts,main = 'Wickets ',xlab = 'Wickets ',ylab = 'Frequency',col = 'green')
boxplot(Ave,main = 'Bowling Average ',xlab = 'average ',ylab = 'Frequency',col = 'pink')
boxplot(Econ,main = 'Economy Rate',xlab = 'economy',ylab = 'Frequency',col = 'brown')
boxplot(SR,main = 'Strike Rate ',xlab = 'strike Rate ',ylab = 'Frequency',col = 'blue')
dev.off()


######### PRINCIPAL COMPONENT ANALYSIS ##############
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
corrmatrix1<-cor(mydata2)
corrmatrix1 


############# Bartlett Sphericity Test 
print(cortest.bartlett(corrmatrix1, nrow(mydata2))) 


#############Running PCA using basic commands
A<-eigen(corrmatrix1)
eigenvectors1<-A$vectors
eigenvalues1<-A$values
D<-diag(eigenvalues1) 
eigenvectors1
eigenvalues1
D

### principal componenet loadings
pc1<-principal(mydata2,nfactors = length(mydata2),rotate="none")
pc1

# Interpreting the variance
part.pca<-eigenvalues1/sum(eigenvalues1)*100
part.pca

#################### Screeplots ##############
plot(eigenvalues1,type="lines",xlab="Pincipal Components",ylab="Eigen Values")
###
ee<-sqrt(eigenvalues1)
pcaloading <-eigenvectors1*ee
pcaloading

# Principal Components Scoring and Perceptual Map
RKPsc<-scale(mydata2)
z<-as.matrix(RKPsc%*%eigenvectors1)
z
pc.cr<-princomp(RKPsc,cor=TRUE)
summary(pc.cr)
# Check for Cumulative Proportion
plot(pc.cr)
biplot(pc.cr)


####fa.sort(z,polar=FALSE)
str(z)
summary(z)
names(z)
scoreb<-z$v1
class(z)
PC1_score<-z[,1]

bowl$PC1_score<-PC1_score
head(bowl)

newdata <- bowl[order(-PC1_score),] 
