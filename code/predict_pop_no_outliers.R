library(readr)
arrests_total <- read_csv("Documents/SchoolWork/412/merged_arr_pov_total.csv")
arrests_18 <- read_csv("Documents/SchoolWork/412/merged_arr_pov_18.csv")
arrests_total <- arrests_total[-c(5,9,10,14,21,31,32,33,39 , 6 , 25),] #get rid of states with low info and predict for Colorado and MISSISSIPPI
arrests_18 <- arrests_18[-c(5,9,10,14,21,31,32,33,39 , 6 , 25),] #get rid of states with low info and predict for Colorado and MISSISSIPPI

variable_CO_18 <- data.frame(Total.all..classes = c(9006))
variable_CO_total <- data.frame(Total.all..classes = c(154110))
variable_MS_18 <- data.frame(Total.all..classes = c(2083))
variable_MS_total <- data.frame(Total.all..classes = c(49859))

model1 <- lm(population ~ Total.all..classes , data = arrests_18)
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
#Note that anything higher than 2p/n would be considered a high leverage point (2*2/40 = 0.1)  
leverage_cutoff <- 0.1
high_leverage <- arrests_18[leverage > leverage_cutoff,]
#Texas and Wisconsin have high leverage - these are influential points

#outliers
library(car)
outlierTest(model1)
#Texas and Wisconsin are outliers (42,48)

model2 <- lm(population ~ Total.all..classes, data = arrests_total)
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
#Note that anything higher than 2p/n would be considered a high leverage point (2*2/40 = 0.1)  
leverage_cutoff <- 0.1
high_leverage <- arrests_total[leverage > leverage_cutoff,]
#Texas and Tennessee have high leverage - these are influential points

#outliers
library(car)
outlierTest(model2)
#Texas is an outlier (42)

#With outliers
abs(predict(model1, newdata = variable_CO_18) - 5678716)
abs(predict(model1, newdata = variable_MS_18) - 1663780)

abs(predict(model2, newdata = variable_CO_total) - 5678716)
abs(predict(model2, newdata = variable_MS_total) - 1663780)
#Next Goal - apply non parametric model 
#############################################
#############################################
library(MASS)

huber_lm1 <- rlm(population ~ Total.all..classes, data = arrests_18)
summary(huber_lm1)

huber_lm2 <- rlm(population ~ Total.all..classes, data = arrests_total)
summary(huber_lm2)

#Robust
abs(predict(huber_lm1, newdata = variable_CO_18) - 5678716)
abs(predict(huber_lm1, newdata = variable_MS_18) - 1663780)

abs(predict(huber_lm2, newdata = variable_CO_total) - 5678716)
abs(predict(huber_lm2, newdata = variable_MS_total) - 1663780)
################################################
################################################
#linear model without outliers
arrests_18_cleaned <- arrests_18[-c(33,39),]
arrests_total_cleaned <- arrests_total[-c(33),]

model1_clean <- lm(population ~ Total.all..classes, arrests_18_cleaned)
summary(model1_clean)
plot(model1_clean)

model2_clean <- lm(population ~ Total.all..classes, arrests_total_cleaned)
summary(model2_clean)
plot(model2_clean)

#Outliers removed
abs(predict(model1_clean, newdata = variable_CO_18) - 5678716)
abs(predict(model1_clean, newdata = variable_MS_18) - 1663780)

abs(predict(model2_clean, newdata = variable_CO_total) - 5678716)
abs(predict(model2_clean, newdata = variable_MS_total) - 1663780) 


