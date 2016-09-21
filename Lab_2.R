#History 90.01 | QSS 30.05
#9.22.16
                                                                                
#Invoke packages: we will need dplyr, readr, and tidyr
#Remember to install any packages you have not used previously
library(dplyr)
library(readr)
library(tidyr)

#READING IN IPUMS DATA----------------------------------------------------------

#The following code assumes your data file is titled "9_22.csv"
#and is located in a folder titled "data" within your project folder
a <- read_csv('./data/9_22.csv')

#Look at the top of the dataset
head(a)
#Look at the bottom of the dataset
tail(a)

#RECODING VARIABLES---------------------------------------------------------------

#For variables like STATEFIP, SEX, RACE, and RACED,
#we want to have the name of the value (e.g. "New Hampshire")
#rather than a numeric code so we don't have to keep looking
#up the codes in the codebook. We can do this by turning 
#these numeric variables into either character variables or
#factor variables. Let's look at the difference:

#In a character variable, the value R stores is the character string.
#Character variable for SEX
b <- a %>% mutate(SEXC=ifelse(SEX==1,'male', 'female'))
#Alternatively
b <- a %>% mutate(SEXC=ifelse(SEX==2,'female', 'male'))
head(b)

#In a factor variable, R stores a numeric value and a set of
#strings that correspond to the numeric values, just like a codebook.
#Factor SEX
c <- b %>% mutate(SEXF=factor(SEX,labels=c('male', 'female')))
head(c)
#The factor function takes two arguments: the variable that is 
#being factored and a vector of value labels, in numeric order.

#Factor RACE
d <- c %>% mutate(RACEF=factor(RACE,labels=c('white', 'black', 
                  'American Indian or Alaska Native', 'Chinese', 'Japanese', 
                  'Other Asian or Pacific Islander', 'other', 'two races', 
                  'three or more races')))
#What is wrong with this code?
#During the years represented by our dataset, not all of these values are used.
#We need to figure out which values are used and limit the label vector to those values.

#Determine which RACE values are present in the dataset
table(c$RACE)

#Factor RACE using the correct value labels
d <- c %>% mutate(RACEF=factor(RACE,labels=c('white', 'black', 
                  'American Indian or Alaska Native', 'Chinese', 
                  'Japanese', 'Other Asian or Pacific Islander', 'other')))
head(d)

#Character variable for RACE
e <- d %>% mutate(RACEC=ifelse(RACE==1,'white',
                  ifelse(RACE==2,'black',
                  ifelse(RACE==3,'American Indian or Alaska Native',
                  ifelse(RACE==4,'Chinese',
                  ifelse(RACE==5,'Japanese',
                  ifelse(RACE==6,'Other Asian or Pacific Islander', 
                  'other')))))))
head(e)

#What do we do if a variable has a LOT of different values, 
#like STATEFIP or RACED?
#We use the codebook to make a crosswalk.

#Read in crosswalk for statefip, creating a dataframe titled "states"
states <- read_csv('./data/statefip.csv')

#Add state names to dataframe with the left_join function
f <- left_join(e,states,by='STATEFIP')
head(f)

#AGGREGATING DATA---------------------------------------------------------------

#Use group_by() and summarise() to determine population by year
g <- f %>% group_by(YEAR) %>% summarise(NUMBER = sum(PERWT))
head(g)

#Population by year and sex
h <- f %>% group_by(YEAR, SEX) %>% summarise(NUMBER = sum(PERWT))
h

#Population by year and sex
h <- f %>% group_by(YEAR, SEXF) %>% summarise(NUMBER = sum(PERWT))
h

#Alternatively
i <- f %>% group_by(SEXF, YEAR) %>% summarise(NUMBER=sum(PERWT))
i

#DATA TABLES FOR DOCUMENTS------------------------------------------------------
#Columns for each gender
library(tidyr)
j <- i %>% spread(SEXF, NUMBER)
j

#Alternatively
j <- h %>% spread(SEXF, NUMBER)
j

#Export to .csv
write_csv(j,'./data/year_sex.csv')

#Population by regions
#start with data frame f
head(f)
#create REGION variable
k <- f %>% mutate(REGION=ifelse(STATEFIP %in% c(2, 15),'Alaska_Hawaii',
                  ifelse(STATEFIP %in% c(4, 6, 8, 16, 30, 32, 35, 41, 49, 53, 56), 'West',
                  ifelse(STATEFIP %in% c(17, 18, 19, 20, 26, 27, 29, 31, 38, 39, 46, 55), 'Midwest',
                  ifelse(STATEFIP %in% c(9, 23, 25, 33, 34, 36, 42, 44, 50),'Northeast','South')))))
head(k)
#calculate population of each region at each census
l <- k %>% group_by(YEAR, REGION) %>% summarise(NUMBER = sum(PERWT))
l
#spread data frame: row for each year, column for each region
m <- l %>% spread(REGION, NUMBER)
m
#alternatively
m <- k %>% group_by(YEAR, REGION) %>% summarise(NUMBER = sum(PERWT)) %>% 
  spread(REGION, NUMBER)
m

#Always exclude Alaska and Hawaii before 1960,
#unless you are asking questions specifically abou those territories
aa <- a %>% filter(!(YEAR < 1960 & STATEFIP %in% c(2, 15)))
aaa <- a %>% filter(YEAR >= 1960 | !(STATEFIP %in% c(2, 15)))

