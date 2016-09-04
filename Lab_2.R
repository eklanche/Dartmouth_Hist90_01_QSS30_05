#Script for Lab 2, 9.22.16

#Invoke packages
library(dplyr)
library(readr)

#Read in IPUMS data
a <- read_csv('./data/9_22.csv')

#Look at the top of the dataset
head(a)
#Look at the bottom of the dataset
tail(a)

#Factor SEX
b <- a %>% mutate(SEXF=factor(SEX,labels=c('male','female')))
head(b)

#Factor RACE
c <- b %>% mutate(RACEF=factor(RACE,labels=c('white','black','American Indian or Alaska Native',
                                             'Chinese','Japanese','Other Asian or Pacific Islander',
                                             'other','two races','three or more races')))

#Determine which RACE values are present in the dataset
table(b$RACE)

#Factor RACE
c <- b %>% mutate(RACEF=factor(RACE,labels=c('white','black','American Indian or Alaska Native',
                                             'Chinese','Japanese','Other Asian or Pacific Islander',
                                             'other')))

#Character variable for SEX
d <- c %>% mutate(SEXC=ifelse(SEX==1,'male','female'))
#Alternatively
d <- c %>% mutate(SEXC=ifelse(SEX==2,'female','male'))
head(d)

#Character variable for RACE
e <- d %>% mutate(RACEC=ifelse(RACE==1,'white',
                        ifelse(RACE==2,'black',
                        ifelse(RACE==3,'American Indian or Alaska Native',
                        ifelse(RACE==4,'Chinese',
                        ifelse(RACE==5,'Japanese',
                        ifelse(RACE==6,'Other Asian or Pacific Islander','other')))))))
head(e)

#Read in crosswalk for statefip
states <- read_csv('./data/statefip.csv')

#Add state names to dataframe
f <- left_join(e,states,by='STATEFIP')
head(f)

#Use group_by() and summarise() to determine population by year
g <- f %>% group_by(YEAR) %>% summarise(NUMBER=sum(PERWT))
head(g)

#Population by year and sex
h <- f %>% group_by(YEAR,SEX) %>% summarise(NUMBER=sum(PERWT))
h

#Population by year and sex
h <- f %>% group_by(YEAR,SEXF) %>% summarise(NUMBER=sum(PERWT))
h

#Alternatively
i <- f %>% group_by(SEXF,YEAR) %>% summarise(NUMBER=sum(PERWT))
i

#Columns for each gender
library(tidyr)
j <- i %>% spread(SEXF,NUMBER)
j

#Alternatively
j <- h %>% spread(SEXF,NUMBER)
j

#Export to .csv
write_csv(j,'./data/year_sex.csv')

#Population by regions
#start with data frame f
head(f)
#create REGION variable
k <- f %>% mutate(REGION=ifelse(STATEFIP %in% c(2,15),'Alaska_Hawaii',
                         ifelse(STATEFIP %in% c(4,6,8,16,30,32,35,41,49,53,56),'West',
                         ifelse(STATEFIP %in% c(17,18,19,20,26,27,29,31,38,39,46,55),'Midwest',
                         ifelse(STATEFIP %in% c(9,23,25,33,34,36,42,44,50),'Northeast','South')))))
head(k)
#calculate population of each region at each census
l <- k %>% group_by(YEAR,REGION) %>% summarise(NUMBER=sum(PERWT))
l
#spread data frame: row for each year, column for each region
m <- l %>% spread(REGION,NUMBER)
m
#alternatively
m <- k %>% group_by(YEAR,REGION) %>% summarise(NUMBER=sum(PERWT)) %>% spread(REGION,NUMBER)
m

#Always exclude Alaska and Hawaii before 1960
aa <- a %>% filter(!(YEAR < 1960 & STATEFIP %in% c(2,15)))
aaa <- a %>% filter(YEAR >= 1960 | !(STATEFIP %in% c(2,15)))

