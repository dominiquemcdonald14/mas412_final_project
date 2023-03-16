library(readr)
arrests_total <- read_csv("Documents/SchoolWork/412/arrests_total.csv")
arrests_18 <- read_csv("Documents/SchoolWork/412/arrests_18.csv")
arrests_total <- arrests_total[-c(9,10),] #get rid of 
arrests_18 <- arrests_18[-c(9,10),]

model1 <- lm(arrests_18$`2021
estimated 
population` ~ arrests_18$`Total
all 
classes`)
summary(model1)
plot(model1)

#Note that with a linear regression, we have high R squared and total all cases crime is significant 
#In checking the assumptions for linear regression however:
#Constant Variance - Variance increases among residuals as fitted values increase - bad
#Normality Assumption - Splits from qqnorm occur at the tail end of the QQ plot of residuals - bad
#Leverage Points - 
#Large Leverage points
leverage <- hatvalues(model1)
plot(hatvalues(model1), type = "h")
#Note that anything higher than 2p/n would be considered a high leverage point (2*2/50 = 0.08)  
leverage_cutoff <- 0.08
high_leverage <- arrests_18[leverage > leverage_cutoff,]
#Texas and Wisconsin have high leverage - these are influential points

#outliers
library(car)
outlierTest(model1)
#Texas and Wisconsin are outliers

model2 <- lm(arrests_total$`2021
estimated 
population` ~ arrests_total$`Total
all 
classes`)
summary(model2)
plot(model2)

#Note that with a linear regression, we have high R squared and total all cases crime is significant 
#In checking the assumptions for linear regression however:
#Constant Variance - Variance increases among residuals as fitted values increase - bad
#Normality Assumption - Splits from qqnorm occur at the tail end of the QQ plot of residuals - bad
#Leverage Points - 
#Large Leverage points
leverage <- hatvalues(model2)
plot(hatvalues(model2), type = "h")
#Note that anything higher than 2p/n would be considered a high leverage point (2*2/50 = 0.08)  
leverage_cutoff <- 0.08
high_leverage <- arrests_total[leverage > leverage_cutoff,]
#Texas and Tennessee have high leverage - these are influential points

#outliers
library(car)
outlierTest(model2)
#Texas and Tennessee are outliers

#Next Goal - apply non parametric model 

