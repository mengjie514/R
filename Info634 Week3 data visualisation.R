##############################
## Week3 Data Visualization ##
##############################
library(ggplot2)
library(readr)
library(dplyr)

#############
## Element ## 
#############
url <- 'http://people.terry.uga.edu/rwatson/data/carbonMeans.txt'
carbon <- read_delim(url, delim = ",")
carbon$relCO2 <- (carbon$CO2-280)/280

# select year(x) and CO2(y) to create a x-y point plot; specify red points
ggplot(carbon, aes(year, CO2)) + geom_point(color = "red")

# add some axes labels (point)
ggplot(carbon, aes(year, CO2)) + 
  geom_point(color = 'red') + 
  xlab('Year') + 
  ylab("CO2 ppm of the atmo") +
  ylim(0,400)

# new column (line)
ggplot(carbon, aes(year, relCO2)) + 
  geom_line(color = "red") +
  xlab("year") + 
  ylab("haha") + 
  ylim(0,.5)

##############
## Exercise ##
##############
# Create a line plot using the data in the following table
year <- c(1804, 1927, 2046)
population <- c(1,2,9)
tb <- tibble(year, population)
ggplot(tb, aes(year, populaiton)) + geom_line()

###########################
## Histogram - count obs ##
###########################
library(measurements)
fpath <- "\\\\file/UsersM$/mwa150/Home/Desktop/Info634/data/centralparktemps.txt"
t <- read_delim(fpath, delim = ",")
t$C <- conv_unit(t$temperature, "F", "C")
ggplot(t, aes(C)) + 
  geom_histogram(fill = "light blue") + 
  xlab("Celsius")

###########################
## Bar chart - count obs ##
###########################
library(DBI)
library(RMySQL) 
# SQL Access # Error
conn <- dbConnect(RMySQL::MySQL(), "richardtwatson.com",
                  dbname="ClassicModels", user="db1", password="student")
d <- dbGetQuery(conn, "SELECT * from Products;")

ggplot(d, aes(x=productLine)) + 
  geom_bar(fill='chocolate') + 
  coord_flip()

# report counts 
# %>% works like a pipe, "chain" multiple functions together
da <- d %>%
  group_by(productLine) %>% 
  summarize(count = n())
ggplot(d, aes(productLine)) + 
  geom_col(fill = "gold")

################
## Radar plot ## 
################
d <- dbGetQuery(conn, "SELECT productLine from Products;")
ggplot(d, aes(x=productLine)) + 
  geom_histogram(fill="gold") + 
  coord_polar + 
  ggtitle("Number of products in each product line") +
  expand_limits(x=c(0,10))

##################
## Scatter plot ##
##################
library(RMySQL)
library(DBI)
library(lubridate)
conn <- dbConnect(RMySQL::MySQL(), "richardtwatson.com", dbname="Weather",
                  user="db2", password="student")
t <- dbGetQuery(conn,"SELECT timestamp, airTemp from record;")
t$year <- year(t$timestamp)
t$month <- month(t$timestamp)
t$hour <- hour(t$timestamp)

t2 <- t %>% filter(year == 2011 & hour == 17 & month == 8)
ggplot(t2, aes(day(time), airTemp)) + 
  geom_point()

############
## Smooth ##
############
url <- 'http://people.terry.uga.edu/rwatson/data/centralparktemps.txt'
t <- read_delim(url, delim = ",")
t2 <- t %>% filter(month == 8)
ggplot(t2, aes(year, temperature)) + 
  geom_line() + 
  geom_smooth()

##############
## Exercise ##
##############
library(ggpubr)
df <- read.csv("gdp&fertility.csv")

cor.test(df$GDP, df$Fertility)

ggplot(df, aes(GDP, Fertility)) +
  geom_point() + 
  geom_smooth()

ggplot(df, aes(log(GDP), log(Fertility))) +
  geom_point() + 
  geom_smooth()

##############
## Box plot ## 
##############
conn <- dbConnect(RMySQL::MySQL(), "richardtwatson.com",
                  dbname="ClassicModels", user="db1", password="student")
d <- dbGetQuery(conn,"SELECT * from Payments;")

# plot amounts paid 
ggplot(d, aes(factor(0), amount)) + 
  geom_boxplot(outlier.colour = "gold") +
  xlab("") + 
  ylab("check")

##############
## Heat Map ##
##############
d <- dbGetQuery(conn, "SELECT * from Products;")
da <- d %>% group_by(productLine, productScale) %>%
  summarise(count=n())

ggplot(da, aes(productLine, productScale)) + 
  geom_title(aes(fill=count)) + 
  scale_fill_gradient(low="light blue", high = "dark blue") + 
  xlab("scale") +
  ylab("line")

##############
## Geo Data ##
##############
library(ggmap)
library(mapproj)
conn <- dbConnect(RMySQL::MySQL(), "richardtwatson.com",
                  dbname="ClassicModels", user="db1", password="student")

# Google maps requires lon and lat, in that order, to create markers
d <- dbGetQuery(conn,"SELECT y(officeLocation) AS lon, x(officeLocation) ASlat FROM Offices;")

# show offices in the United States
# vary zoom to change the size of the map
map <- get_googlemap('united states',marker=d,zoom=4)
ggmap(map) +
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle('US offices')

## Broadwick St ## 
url <- 'http://people.terry.uga.edu/rwatson/data/pumps.csv'
pumps <- read_delim(url, delim=',')
url <- 'http://people.terry.uga.edu/rwatson/data/deaths.csv'
deaths <- read_delim(url, delim=',')

map <- get_googlemap('broadwick street, london, united kingdom',markers=pumps,zoom=15)
ggmap(map) + labs(x = 'Longitude', y = 'Latitude') + ggtitle('Pumps and deaths') +
  geom_point(aes(x=longitude,y=latitude,size=count),color='blue',data=deaths) +
  xlim(-.14,-.13) + ylim(51.51,51.516)