#9.29.16
#History 90.01 | QSS 30.05

library(dplyr)
library(readr)
library(ggplot2)
library(RColorBrewer)

#Read in IPUMS data; filter out Alaska and Hawaii; limit to ages 15-65
a <- read_csv('/users/emily/dropbox/hist90_01/Fall2016/labs/9_29/9_29.csv') %>%
  filter(AGE>=15 & AGE<=65 & !(STATEFIP %in% c(2,15)))

#Recode region, industry, and sex
b <- a %>% mutate(REGION=factor(
                          ifelse(STATEFIP %in% c(4, 6, 8, 16, 30, 32, 35, 41, 49, 53, 56), 4,
                          ifelse(STATEFIP %in% c(17, 18, 19, 20, 26, 27, 29, 31, 38, 39, 46, 55), 3,
                          ifelse(STATEFIP %in% c(9, 23, 25, 33, 34, 36, 42, 44, 50), 1, 2))),
                          labels=c('Northeast','South','Midwest','West')),
                  IND=factor(
                      ifelse(IND1950<100 | IND1950>976, 1,
                      ifelse(IND1950<246, 2,
                      ifelse(IND1950==246 | IND1950==976,4,
                      ifelse(IND1950>700,7,
                      ifelse(IND1950>600,6,
                      ifelse(IND1950>500,5,3)))))),
                      labels=c('none','agricultural/extractive','manufacturing',
                               'construction or general labor',
                               'transporation/communication/utilities',
                               'trade','service')),
                  SEXC=ifelse(SEX==1,'male','female'))

c <- b %>% group_by(YEAR,IND,REGION,SEXC) %>% summarise(NUMBER=sum(PERWT))

png('ind_region.png',height=500,width=1000)
ggplot(data=c,aes(x=YEAR,y=NUMBER,fill=IND)) + 
  geom_bar(stat='identity',position='fill') + 
  labs(x='Year',y='Percent',fill='Occupation',title='Occupation by Sex, Region, and Year, 1870-1920') +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='Set1',guide=guide_legend(reverse=TRUE)) +
  facet_grid(SEXC~.~REGION) +
  theme_bw(base_size = 22)
dev.off()

png('region_sex.png',height=1000,width=1000)
ggplot(data=arrange(c,SEXC),aes(x=YEAR,y=NUMBER,fill=SEXC)) +
  geom_bar(stat='identity') +
  labs(x='Year',y='Number',fill='Sex',title='Population by Region, Year, and Sex, 1870-1920') +
  scale_y_continuous(labels=scales::comma) +
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='Set2',guide=guide_legend(reverse=TRUE)) +
  facet_wrap(~REGION,ncol=2,scales='free_y') +
  theme_bw(base_size = 24)
dev.off()