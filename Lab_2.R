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
