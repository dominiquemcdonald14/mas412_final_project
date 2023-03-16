library(readr)
arrests_total <- read_csv("Documents/SchoolWork/412/merged_arr_pov_total.csv")
arrests_18 <- read_csv("Documents/SchoolWork/412/merged_arr_pov_18.csv")
arrests_total <- arrests_total[-c(9,10),] #get rid of DC and Florida 
arrests_18 <- arrests_18[-c(9,10),]

variable_DC_18 <- data.frame(Total.all..classes = c(148))
variable_DC_total <- data.frame(Total.all..classes = c(2002))

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
#Note that anything higher than 2p/n would be considered a high leverage point (2*2/50 = 0.08)  
leverage_cutoff <- 0.08
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
#Note that anything higher than 2p/n would be considered a high leverage point (2*2/50 = 0.08)  
leverage_cutoff <- 0.08
high_leverage <- arrests_total[leverage > leverage_cutoff,]
#Texas and Tennessee have high leverage - these are influential points

#outliers
library(car)
outlierTest(model2)
#Texas and Tennessee are outliers (41,41)

#With outliers
predict(model1, newdata = variable_DC_18)
predict(model2, newdata = variable_DC_total)
#Next Goal - apply non parametric model 
#############################################
#############################################
library(MASS)

huber_lm1 <- rlm(population ~ Total.all..classes, data = arrests_18)
summary(huber_lm1)

huber_lm2 <- rlm(population ~ Total.all..classes, data = arrests_total)
summary(huber_lm2)

#Robust
predict(huber_lm1, newdata = variable_DC_18)
predict(huber_lm2, newdata = variable_DC_total)
################################################
################################################
#linear model without outliers
arrests_18_cleaned <- arrests_18[-c(42,48),]
arrests_total_cleaned <- arrests_total[-c(41,42),]

model1_clean <- lm(population ~ Total.all..classes, arrests_18_cleaned)
summary(model1_clean)
plot(model1_clean)

model2_clean <- lm(population ~ Total.all..classes, arrests_total_cleaned)
summary(model2_clean)
plot(model2_clean)

#Outliers removed
predict(model1_clean, newdata = variable_DC_18)
predict(model2_clean, newdata = variable_DC_total)


