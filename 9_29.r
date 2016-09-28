#9.29.16
#History 90.01 | QSS 30.05

#load packages
library(dplyr)
library(readr)
library(ggplot2)
library(RColorBrewer)

#Read in IPUMS data; be sure to correctly specify the path
a <- read_csv('/users/emily/documents/Hist90_01_QSS30_05/9_29.csv') #full path
a <- read_csv('9_29.csv') #path to data in your project folder
a <- read_csv('data/9_29.csv') #path to data in "data" folder in project folder

#If you are not working within a "project", 
#you can specify a working directory
setwd('/users/emily/documents')
#then you can refer directly to documents in that directory
a <- read_csv('9_29.csv')
#Or you can specify the full path without setting working directory
a <- read_csv('/users/documents/9_29.csv')

#Read in IPUMS data; filter out Alaska and Hawaii; limit to ages 15-65
a <- read_csv('data/9_29.csv') %>%
  filter(AGE>=15 & AGE<=65 & !(STATEFIP %in% c(2,15)))
#Check to see if it worked
a %>% filter(AGE<15 | AGE>65 | STATEFIP %in% c(2,15))

#Variables in my data frame: YEAR, STATEFIP, PERWT, 
#SEX, AGE, RACE, RACED, OCC1950, IND1950
#Variables I want in my graphs: YEAR, Region (new),
#Sex (recoded), Industry (recoded)
#I also need to keep PERWT for aggregating

#Create Region variable from STATEFIP
#I can do this as a character or a factor
b <- a %>% mutate(Region=ifelse(STATEFIP %in% c(4, 6, 8, 16, 30, 32, 35, 41, 49, 53, 56), 'West',
                         ifelse(STATEFIP %in% c(17, 18, 19, 20, 26, 27, 29, 31, 38, 39, 46, 55), 'Midwest',
                         ifelse(STATEFIP %in% c(9, 23, 25, 33, 34, 36, 42, 44, 50), 'Northeast', 'South'))))
b <- a %>% mutate(Region=factor(ifelse(STATEFIP %in% c(4, 6, 8, 16, 30, 32, 35, 41, 49, 53, 56), 4,
                                ifelse(STATEFIP %in% c(17, 18, 19, 20, 26, 27, 29, 31, 38, 39, 46, 55), 3,
                                ifelse(STATEFIP %in% c(9, 23, 25, 33, 34, 36, 42, 44, 50), 1, 2))),
                          labels=c('Northeast','South','Midwest','West')))

#Create Industry variable by recoding IND1950
c <- b %>% mutate(Industry=factor(ifelse(IND1950<100 | IND1950>976, 1,
                                  ifelse(IND1950<246, 2,
                                  ifelse(IND1950==246 | IND1950==976,4,
                                  ifelse(IND1950>700,7,
                                  ifelse(IND1950>600,5,
                                  ifelse(IND1950>500,6,3)))))),
                            labels=c('none','agricultural/extractive','manufacturing',
                                      'construction or general labor','trade',
                                      'transportation/communication/utilities',
                                      'service')))

#Recode SEX
d <- c %>% mutate(Sex=ifelse(SEX==1,'male','female'))

#Keep only variables I will use for graphing
#This step is not necessary, but lets me see
#that I have all of the variables I need
e <- d %>% select(YEAR,PERWT,Region,Industry,Sex)
head(e)

#I'm planning to make two graphs. 
#For the first one, I want to group by YEAR, Sex, and Region. 
f1 <- e %>% group_by(YEAR, Sex, Region) %>% summarise(Number=sum(PERWT))
#For the second one, I want to group by YEAR, Sex, Region, and Industry.
f2 <- e %>% group_by(YEAR, Sex, Region, Industry) %>% summarise(Number=sum(PERWT))

png('ind_region.png',height=500,width=1000)
ggplot(data=c,aes(x=YEAR,y=NUMBER,fill=IND)) + 
  geom_bar(stat='identity',position='fill') + 
  labs(x='Year',y='Percent',fill='Industry',title='3. Industry for Persons Aged 15-65 by Sex, Region, and Year, 1870-1920') +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='Set1') +
  facet_grid(SEXC~.~REGION) +
  theme_bw(base_size = 18) + theme(legend.position='bottom')
dev.off()

png('region_sex.png',height=500,width=1000)
ggplot(data=arrange(c,SEXC),aes(x=YEAR,y=NUMBER,fill=SEXC)) +
  geom_bar(stat='identity') +
  labs(x='Year',y='Number',fill='Sex',title='1. Population Aged 15-65 by Region, Year, and Sex, 1870-1920') +
  scale_y_continuous(labels=scales::comma) +
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='Set2',guide=guide_legend(reverse=TRUE)) +
  facet_wrap(~REGION,ncol=2,scales='free_y') +
  theme_bw(base_size = 18)
dev.off()