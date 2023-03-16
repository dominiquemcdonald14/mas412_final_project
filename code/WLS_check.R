library(readr)
arrests_total <- read_csv("Documents/SchoolWork/412/merged_arr_pov_total.csv")
arrests_18 <- read_csv("Documents/SchoolWork/412/merged_arr_pov_18.csv")
arrests_total <- arrests_total[-c(9,10),] #get rid of DC and Florida 
arrests_18 <- arrests_18[-c(9,10),]


##########################################################
##########################################################
#Weighted Least Squares
model1 <- lm(population ~ Total.all..classes , data = arrests_18)
summary(model1)
plot(model1)
#define weights to use
wt1 <- 1 / lm(abs(model1$residuals) ~ model1$fitted.values)$fitted.values^2
#perform weighted least squares regression
wls_model <- lm(population ~ Total.all..classes , data = arrests_18, weights = wt1)
#view summary of model
summary(wls_model)
plot(wls_model)



model2 <- lm(population ~ Total.all..classes, data = arrests_total)
summary(model2)
plot(model2)
#define weights to use
wt2 <- 1 / lm(abs(model2$residuals) ~ model2$fitted.values)$fitted.values^2
#perform weighted least squares regression
wls_model2 <- lm(population ~ Total.all..classes , data = arrests_total, weights = wt2)
#view summary of model
summary(wls_model2)
plot(wls_model2)
