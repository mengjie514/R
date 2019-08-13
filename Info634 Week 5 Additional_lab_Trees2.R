## 2019.8.13
## by Mengjie Wang

##################################
## Using training and test data ##
##################################

t <- read.csv('breast-cancer-wisconsin.csv')
t <- within(t, {
  class[class == 2] <- 'Benign'
  class[class == 4] <- 'Malignant'})

# Set seed of random number generator
set.seed(1000)

# Ensure that Class is categorical
is.factor(t$class)
t$class <- as.factor(t$class)

# Split the data
# 75% of values are used as training data
# The remaining 25% are used as test data
N <- nrow(t)
indtrain <- sample(1:N,size=0.75*N)
indtrain <- sort(indtrain)
indtest <- setdiff(1:N,indtrain)

# Fit a classifier to only the training data
fit.r <- rpart(class~.,data=t,subset=indtrain)

# Classify for ALL of the observations
pred <- predict(fit.r,type="class",newdata=t)

# Look at table for the test data only (rows=truth, cols=prediction)
tab <- table(t$class[indtest],pred[indtest])
tab

# Work out the accuracy
sum(diag(tab))/sum(tab)

# Look at the results for the training data only
tab <- table(t$class[indtrain],pred[indtrain])
tab

# Work out the accuracy
sum(diag(tab))/sum(tab)
