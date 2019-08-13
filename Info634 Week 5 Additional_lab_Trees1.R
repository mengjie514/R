## 2019.8.13
## by Mengjie Wang

#######################
## Read Titanic Data ##
#######################

setwd("")
titanic <- read.csv("titanic.txt", sep = "\t", header = TRUE)

# Recode Response variable
titanic <- within(titanic, {
  Survived[Survived==0] <- 'no'
  Survived[Survived==1] <- 'yes'
  })

###############################
## Exploratory data analysis ##
###############################

# What percent survived? 
titanic %>% 
  group_by(Survived) %>%
  summarise(count = n()) %>% 
  mutate(freq = count/sum(count))

# what percent of each gender survived? Does women were more likely to survived than men? 
titanic %>% group_by(Survived, titanic$Sex) %>%
  summarise(count = n()) %>%
  mutate(freq = count/sum(count))

# Or use xtabs funciton 
t <- xtabs(~ Survived + Sex, data = titanic)
prop.table(t, 1)

# Mapping it out (Recall ggplot)
# Suppose - whehter Pclass affect the survival rate, malels and female from differnet class may have different survival rate
library(ggplot2)
ggplot(titanic,aes(x=PClass,fill=Survived)) + 
  geom_bar(position='stack')+ coord_flip() + facet_wrap(~Sex) 

####################
## Decision trees ##
####################

library(rpart)
library(rpart.plot)
formula <- Survived ~ PClass + Sex + Age
fit <- rpart(formula,titanic, method = "class") # predicted class; predicted probability of survivals; percentage of observations
rpart.plot(fit, type = 4)

# or use draw.tree; can you find the difference
library(maptree)
require(cluster)
plot(as.party(fit))
draw.tree(fit)

