#############################
## Week2 Introduction to R ##
#############################

## Reading a local text file 
library(readr)
fpath <- "\\\\file/UsersM$/mwa150/Home/Desktop/Info634/data/centralparktemps.txt"
t <- read_delim(fpath, delim = ",")

## Reading a remote text file
url <- 'http://people.terry.uga.edu/rwatson/data/centralparktemps.txt'
t <- read_delim(url,delim=',')

head(t); tail(t)
dim(t) # dimension
str(t) # structure of dataset
class(t) # type of object

#Creating a new column
library(measurements)
t$Ctemp <- round(conv_unit(t$temperature, 'F', 'C'),1)

#Recoding 
library(dplyr)
t$Category <- case_when(t$Ctemp >=25 ~ 'Hot',
                        t$Ctemp < 25 & t$Ctemp >=5 ~ 'Mild',
                        t$Ctemp <5 ~ 'Cold')
#Deleting 
t$Category <- NULL

#Output files 
write_csv(t, "centralparktempsCF.txt")

## Dplyr 
fpath <- "\\\\file/UsersM$/mwa150/Home/Desktop/Info634/data/centralparktemps.txt"
t <- read_delim(fpath, delim = ",")

#Selecting rows and columns
trow <- filter(t, year == 1999) # filter columns with certain values
tcol <- select(t, year) # select rows
trowcol <- t %>% select(year, month, temperature) %>% filter(year > 1989 & year <2000) # select rows and columns

#sorting 
arrange(t, desc(year), month)

#summarizing 
mean(t$temperature)
summarise(t, mean(temperature))
w <- t %>% group_by(year) %>% summarise(averageF = mean(temperature))
w

#mutating (add col)
t <- mutate(t, Ctemp = (temperature-32)*5/9)


## Merging files 
a <- t %>% group_by(year) %>% summarise(mean = mean(temperature))

fpath <- "\\\\file/UsersM$/mwa150/Home/Desktop/Info634/data/carbon.txt"
carbon <- read_delim(fpath, delim = ",")

m <- inner_join(a, carbon)

## Correlation
cor.test(m$mean,m$CO2)
## Linear regression
summary(lm(m$mean ~ m$CO2))

## Read a spreadsheet
setwd("\\\\file/UsersM$/mwa150/Home/Desktop/Info634/data")
library(readxl)
InternetCompanies <- read_excel("InternetCompanies.xlsx")

## MYSQL
library(DBI)
Library(RMySQL)
conn <- dbConnect(RMySQL::MySQL(), "richardtwatson.com",
                  dbname="Weather", user="db2", password="student")

# Query the database and create file t for use with R
t <- dbGetQuery(conn,"SELECT * from record;")
head(t)

## Exercise
data <- read.csv("Product.csv")
head(data)
new_data <- data %>% group_by(PRODUCTLINE) %>% summarise(mean(QUANTITYORDERED))
write.csv("new_data.csv")
data %>% group_by(PRODUCTLINE) %>% summarise(sum(QUANTITYORDERED))
