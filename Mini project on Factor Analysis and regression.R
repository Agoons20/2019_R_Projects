###Mini project on Factore Analysis & Regression 

setwd("C:/Users/Myller/Desktop/PGP - BABI/Week 11 Advanced Statistics Project")
getwd()
Hair_data=read.csv("Factor-Hair-Revised.csv", header = TRUE)

## Importing packages
library(corrplot)     # used for making correlation plot.
library(tidyverse)    # metapackage with lots of helpful functions
library(ggplot2)    # used for plotting
library(psych)     # used for Factor Analysis
library(car)      # used for calculating VIF (Variance Inflation Factor) to check multicollinearity
install.packages("caTools")
library(caTools)    # used for partitioning the data
library(DataExplorer)  #Used for creating preliminar data report and other visuals


### Exloratory data analysis
View(Hair_data)
names(Hair_data)
head(Hair_data)
tail(Hair_data)
dim(Hair_data)
str(Hair_data) #We need to visualize this. The DataExplorer library together with the create
plot_str(Hair_data)
summary(Hair_data)

## From the structure of our dataset, we noticed that the column, ID is of data type integer. Thus, we delete it. 
## We won't be needing it in our analysis. 

Hair_data=Hair_data[,2:13]
View(Hair_data)


## Saving Variable names in matrix . This code changes the names of our variables
variables=c("Product Quality" , "E-Commerce" , "Technical Support" , "Complaint Resolution" , 
            "Advertising" , "Product Line" , "Salesforce Image", "Competitive Pricing" , 
            "Warranty & Claims" , "Order & Billing" , "Delivery Speed" , "Customer Satisfaction")

## Change names of variables 
colnames(Hair_data)= variables
attach(Hair_data)
dim(Hair_data)

## Missing Values Identification
anyNA(Hair_data) #Our dataset has no missing values

library(DataExplorer)
create_report(Hair_data)

### Missing Value Identification --> this tells us if we have missing values in dataset graphically
plot_intro(Hair_data)
plot_missing(Hair_data)
### Our dataset is ready for analysis 


### Univariate Analysis 

## Histogram of our Dependent Variable (Customer Satisfaction)
hist(`Customer Satisfaction`,col = "Grey")


## Boxplot of our Dependent Variable (Customer Satisfactiion)
boxplot(`Customer Satisfaction`, horizontal = TRUE,main="Boxplot of Customer Satisfaction")  
# Boxplot is left skewed


## Histogram of the independent Variables. We will convert Plotting space in for 12 vriables
par(mfrow = c(3,3)) 
hist(`Product Quality`,col = "green")
hist(`E-Commerce`,col = "green")
hist(`Technical Support`,col = "green")
hist(`Complaint Resolution`,col = "pink")
hist(Advertising,col = "pink")
hist(`Product Line`,col = "pink")
hist(`Salesforce Image`,col = "orange")
hist(`Competitive Pricing`,col = "orange")
hist(`Warranty & Claims`,col = "orange")
hist(`Order & Billing`,col="light blue")
hist(`Delivery Speed`,col = "light blue")
dev.off()  #helps to reset our plotting space

### Boxplot for independent variables
boxplot(Hair_data[,-12], las = 2, names = variables[-12], cex.axis = 1)
# We take away the 12th column from the boxplot

## Finding Outliers in variables 
list("OutLiers")
OutLiers =Hair_data[(1:12),]
for (i in c(1:12)) {
  
  Box_Plot =boxplot(Hair_data[,i],plot = F)$out
  OutLiers[,i] <- NA
  
  if (length(Box_Plot)>0) {
    OutLiers[(1:length(Box_Plot)),i] <- Box_Plot 
  }
}
# Write outliers list in csv
write.csv(OutLiers, "OutLiers.csv")



#### Bivariate Analysis ####
#### Scatter Plot of independent variables against the Dependent Variable
par(mfrow = c(3,3))
for(i in c(1:11)) {
  plot(Hair_data[,i],`Customer Satisfaction`, 
       xlab = variables[i], ylab = NULL, col = "red", 
       cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1,
       xlim = c(0,10),ylim = c(0,10))
  abline(lm(formula = `Customer Satisfaction` ~ Hair_data[,i]),col = "blue")
  
}
dev.off()

### Is there any evidence of multicollinearity?

### Now, we check the correlation of our dataset
CorrHair=cor(Hair_data[,-12]) ### this is not very explicit. lets do something else
CorrHair
library(corrplot)
corrplot(CorrHair,type = "upper")
corrplot(CorrHair,method = "number",type = "upper") #Display Correlation coeficients

## Some variables seem to have a high correlation with others. But as per the conditions of regression
## the independent variables need to be independent of each other. 

### To further validate our exploratory analysis, we do a Barlett Sphericity Test. 
### to confirm whether dimension reduction is possible or not.
cortest.bartlett(CorrHair,nrow(Hair_data))
print(cortest.bartlett(CorrHair,nrow(Hair_data)))

# our p-value < alpha and thus, dimension reduction is possible. 


### C] Finding out the Eigen Values and Eigen Vectors. or we do Eigenvalue extraction. 
### We use the function, "eigen" with argument, "output of correlation matrix"
A=eigen(CorrHair) 
A
## This steps prints the eigen values and prints all the various combinations of factors


### This step prints only the eigenvalues from the above code and other code prints just the eigen vectors
eigenvalues=A$values
eigenvectors=A$vectors
eigenvalues

### Eigenvalues = Principal components
### How do we interprete the Eigenvalues? 
### Interpreting the variance in our dataset
part.pca=eigenvalues/sum(eigenvalues)*100
part.pca
print(cumsum(part.pca))

### the first eigenvalue explains 31.15% of variance in our dataset, 23.19% for the 2nd eigenvalue, 15.37% for 
### the 3rd eigenvalue and 9.88% for the 4th eigenvalue. 

## The 4 PC's are able to explain 80% of Variance (get this value by adding all the part.pca for the 4 PC's)
## This is in essence what the communalities tells us.


## Getting the loadings and Communality
pc=principal(Hair_data[,-12], nfactors = length(Hair_data[,-12]),rotate = "none")
pc 


## Plotting SCREE Graphs

plot(eigenvalues, main = "Scree Plot", xlab = "Factors", ylab = "Eigen Values", pch = 20, col = "Black")
lines(eigenvalues, col = "Red")
abline(h = 1, col = "green", lty = 10)
## We choose 4 PC's from our Scree plot and green line on graph clearly shows us that. 



#===================================================================================================================================

# FACTOR ANALYSIS Q2

#===================================================================================================================================

### First create correlation matrix of data
CorrHair=cor(Hair_data[,-12]) # We take away the dependent variable from the correlation exercise
CorrHair
round(CorrHair,2)


### Kaiser-Meyer-Olkin (KMO) Test is a measure of how suited your data is for Factor Analysis

KMO(r=CorrHair)
## Overall MSA =  0.65 which tells us that our sample is good for factor analysis.


### Factor Analysis using Principal Axis Factoring method using 4 factors as gotten from Scree plot
### we use the functioin "fa" and without rotation.
##  As per the above scree plot extracting 4 factors from 11 variables

FourFactors=fa(r= Hair_data[,-12], nfactors =4, rotate ="none", fm ="pa")
FourFactors

Loading1 = print(FourFactors$loadings,cutoff = 0.3)
write.csv(Loading1, "loading1.csv")
fa.diagram(FourFactors)


## With rotation
FourFactors1=fa(r= Hair_data[,-12], nfactors =4, rotate ="varimax", fm ="pa")
FourFactors1
Loading2 = print(FourFactors1$loadings,cutoff = 0.3)
write.csv(Loading2, "Loading2.csv")
fa.diagram(FourFactors1)


### We do Oblique rotation to get a better diagram with clear separation of previous diagram. 
install.packages("GPArotation")
library(GPArotation)
FourFactors2=fa(r= Hair_data[,-12],nfactors=4,rotate = "oblimin",fm="pa")
FourFactors2
Loading3 = print(FourFactors2$loadings,cutoff = 0.3) 
write.csv(Loading3, "Loading3.csv")
fa.diagram(FourFactors2)


FourFactors2$scores=FourFactors2$scores


### Create a new data.structure using scores for four factors and Dependent variable
Hair_data2 = cbind(Hair_data[,12], FourFactors1$scores)
View(Hair_data2)
head(Hair_data2)
tail(Hair_data2)

## Name the columns for Hair_data2
colnames(Hair_data2) = c("Cust.Satisf", "Sales.Distri", "Marketing","After.Sales.Service","Value.For.Money")
head(Hair_data2)
tail(Hair_data2)
class(Hair_data2)
# Class is matrix. We need to convert it to a "data.frame"

Hair_data2=as.data.frame(Hair_data2)
View(Hair_data2)
class(Hair_data2)
anyNA(Hair_data2)
headTail(Hair_data2)
attach(Hair_data2)
summary(Hair_data2)
corrplot.mixed(cor(Hair_data2), tl.col = "black",tl.pos = "lt")


### Regressin Analysis
model=lm(Cust.Satisf~Sales.Distri+Marketing+After.Sales.Service+Value.For.Money,data = Hair_data2)
summary(model)
anova(model)
## Total sum of squares = 
### Equation estimated from our model
#Yhat=6.91800+0.57963X1+0.61978X2+0.05692X3+0.61168X4
# Yhat = 6.91800 + 0.57963X1 + 0.61978X2 + 0.05692X3 + 0.61168X4

## Interpretation of intercepts  and model validity

## Get confidence intervals to interprete the slopes from a conservative stand point
confint(model,'Sales.Distri')
confint(model,"Marketing")
confint(model,"After.Sales.Service")
confint(model,"Value.For.Money")

## How good is the regression model?
Predicted=predict(model)
Actual=Cust.Satisf
Backtrack=data.frame(Actual,Predicted)
Backtrack
plot(Actual,col="Red")
lines(Actual,col="Red")
plot(Predicted,col="Blue")
lines(Predicted,col="Blue")
