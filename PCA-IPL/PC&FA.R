
#============================================================================
#
#
# PRINCIPAL COMPONENT ANALYSIS
#
#============================================================================
library(psych)
library(car)
library(foreign)
library(MASS)
library(lattice)
library(nortest) # Anderson Darling
setwd("C:/Users/anisha.jagadesan/Downloads")
RKP=read.csv("RKP_Delhi_Edited.csv", header = TRUE)
names(RKP)
str(RKP)
head(RKP)
summary(RKP)
RKP <- na.omit(RKP)
summary(RKP)
str(RKP)
RKPCorr <- cor(RKP[-c(1,2,22)])
RKPCorr
print(cortest.bartlett(RKPCorr,nrow(RKP)))
## Results indicate that the p value is < .001 (not really 0!) and is statistically significant. PCA can be done. 
## When I have data, I do end up checking, inspecting the correlation matrix or use items in a way where I know 
## they will be correlated with each other to some extent.

# Finding out the Eigen Values and Eigen Vectors.
A<-eigen(RKPCorr)
eigenvalues<-A$values
eigenvectors<-A$vectors
eigenvalues
# We will consider Components which are having eigenvalues > 1 unit
# i.e. PC1 - PC5.
#
eigenvectors
# Getting the loadings and Communality
pc<-principal(RKP[-c(1,2,22)],nfactors = length(RKP[-c(1,2,22)]),rotate="none")
pc
# Interpreting the variance
#
part.pca<-eigenvalues/sum(eigenvalues)*100
part.pca
# The 5 PC's are able to explain 75% of Variance.
#Plotting SCREE Graphs
plot(eigenvalues,type="lines",xlab="Pincipal Components",ylab="Eigen Values")
ee<-sqrt(eigenvalues)
pcaloading <-eigenvectors*ee
pcaloading
# Principal Components Scoring and Perceptual Map
RKPsc<-scale(RKP[-c(1,2,22)])
z<-as.matrix(RKPsc%*%eigenvectors)
z
pc.cr<-princomp(RKPsc,cor=TRUE)
summary(pc.cr)
# Check for Cumulative Proportion
plot(pc.cr)
biplot(pc.cr)
#============================================================================
#
#
# FACTOR ANALYSIS
#
#============================================================================
#
RKPCorr<-cor(RKP[-c(1,2,22)])
round(RKPCorr, 2)
#
# Kaiser-Meyer-Olkin (KMO) Test : For finding Measure of Sampling Adequacy
KMO(r=RKPCorr)
## Kaiser-Meyer-Olkin factor adequacy
# Bartlett's Test of Sphericity:
print(cortest.bartlett(RKPCorr,nrow(RKP)))
# Finding out the Eigen Values and Eigen Vectors.
A<-eigen(RKPCorr)
eigenvalues<-A$values
eigenvectors<-A$vectors
eigenvalues
#Plotting SCREE Graphs
plot(eigenvalues,type="lines",xlab="Pincipal Components",ylab="Eigen Values")
# Factor Analysis using Principal Axis Factoring using 5 factors
#
solution<-fa(r=RKPCorr,nfactors=5,rotate = "none",fm="pa")
solution
# Explore Loading if Factors can be balanced.
solution1 <-fa(r=RKPCorr,nfactors=5,rotate = "varimax",fm="pa")
solution1
# Draw the Factor Diagram
fa.diagram(solution1,simple=FALSE)
#============================================================================
#
#
# T H E - E N D
#
#============================================================================