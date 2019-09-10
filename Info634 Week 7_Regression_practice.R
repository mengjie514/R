## Info634 Practice 
## 2019.9.10

##################################################
## Example. T-test & Multiple Linear Regression ##
##################################################
# Body data from a study on the relationship between body dimensions. 
# The study measured 500+ active individuals.
# http://jse.amstat.org/datasets/body.txt 

body.dat <- read.csv("body.csv")
View(body.dat)

body.dat$Gender <- as.factor(body.dat$Gender)
levels(body.dat$Gender)
levels(body.dat$Gender) <- c("Female","Male")

# Q.a Test the hypothesis that men are taller than women on average. Assume a significance of 5%
t.test(body.dat$Height[body.dat$Gender == "Male"], body.dat$Height[body.dat$Gender == "Female"])

# Q.b Test the hypothesis that men are heavier than women on average. Assume a significance of 1%
t.test(body.dat$Weight[body.dat$Gender == "Male"], body.dat$Weight[body.dat$Gender == "Female"], conf.level = .99)

# Q.c BMI is calculated as weight(kg)/(height(m))^2 
# Test the hypothesis that men have a higher BMI than women on average
body.dat$BMI <- body.dat$Weight/(body.dat$Height*body.dat$Height)

attach(body.dat) # the attach function allows to access variables of a data.frame without calling the data.frame
t.test(BMI[Gender == "Male"], BMI[Gender == "Female"])

# Q.d Calculate the regression of Height on the other body measurements for men and women separately. 
# Which measurements are the best predictors of height for each gender?
body.dat <- read.csv("body.csv") # read the original body data (no BMI)
attach(body.dat)

body.male <- body.dat[which(body.dat$Gender == 0),]
body.male$Gender <- NULL
male.fit <- lm(Height ~., data = body.male)
summary(male.fit)

body.female <- body.dat[which(body.dat$Gender == 1), ]
body.female$Gender <- NULL
female.fit <- lm(Height ~., data = body.female)
summary(female.fit)

##############
## Exercise ##
##############

# Question a 
# Using the data 'house.csv' calculate the regression of price on each of the predictors. 
# Which predictor has the greatest effect on house price? Which has the least? Justify your answer using regression output.
# * you may recode the variable 'prefarea' as a factor with levels 0 = "No" and 1 = "Yes"; 
# * you may generate a new variable to represent all features of the house (i.e., driveway, recroom, fullbase, gashw, airco). 

house.dat <- read.csv("Housing.csv", header = T)
house.dat$X <- NULL

# generate a new variable representing the features of house 
house.dat$features <- house.dat$driveway + house.dat$recroom + house.dat$fullbase +
  house.dat$gashw + house.dat$airco 

# recode nominal data as a factor variable
levels(house.dat$prefarea) 
house.dat$prefarea <- as.factor(house.dat$prefarea)
levels(house.dat$prefarea) <- c("No", "Yes")

# if we plot the histogram of price, we see a significant right skew in this data, meaning the mass of cases are bunched at lower values:
hist(house.dat$price)
# if we plot the histogram of the logarithm of price, however, we see a distribution that looks much more like a normal distribution:
hist(log(house.dat$price))

house.fit <- lm(log(price) ~ lotsize + bedrooms + bathrms + stories + features + 
                  prefarea, data = house.dat) 
summary(house.fit)

# Interpretations for 
# Coefficient-Estimate & Pr(>|t|)
# Multiple R-squared and Adjusted R-squared
# Intercept; F-statistic & p-value
# or any other values that may interest you

#######################
## Model Diagnostics ##
#######################
# Question b 
# How do you diagnose the model you used in the previous question, and what you can find out? 

par(mfrow=c(2,2)) 
plot(house.fit)

# Interpretations considering: 
# Residuals vs Fitted: check the linear relationship assumptions
# Normal Q-Q: examine whether the residuals are normally distributed
# Scale-Location (or Spread-Location): check the homogeneity of variance of the residuals (homoscedasticity)
# Residuals vs Leverage: Whether there are extreme data points that might influence the regression results
