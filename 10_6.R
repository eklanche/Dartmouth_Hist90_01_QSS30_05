#Script for 10_6
#Hist 90.01 | QSS 30.05

#Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

#Read in IPUMS data
a <- read_csv('data/10_6.csv')

#Create vector of age category labels
agecats <- '0-9'
for (i in 1:7) {
  agecats <- c(agecats,paste(i,'0-',i,9,sep=''))
}
agecats <- c(agecats,'80+')


b <- a %>% mutate(Sex=factor(SEX,labels=c('Male','Female')))
c <- b %>% mutate(Age=ifelse(AGE>=80,8,floor(AGE/10)))
d <- c %>% mutate(Age=factor(Age,labels=agecats))
e <- d %>% mutate(Gen=ifelse(BPL>150,'First Generation',
                      ifelse(MBPL>150 | FBPL>150,'Second Generation',
                      'Neither')))
f <- e %>% filter(YEAR>=1960 | !(STATEFIP %in% c(2,15)))
g <- f %>% filter(Gen != 'Neither')
g2 <- g %>% mutate(Weight=ifelse(YEAR==1940 & Gen=='Second Generation',
                    SLWT,PERWT))
h <- g2 %>% group_by(Age,Sex,Gen,YEAR) %>% summarise(Number=sum(Weight))
h2 <- h %>% mutate(Number=ifelse(Sex=='Male',-1 *Number,Number))

png('population_pyramid_1.png',height=500,width=2000)
ggplot(data=h2,aes(x=Age,y=Number,fill=Sex)) +
  geom_bar(data=h2[h2$Sex=='Male',], stat='identity') +
  geom_bar(data=h2[h2$Sex=='Female',],stat='identity') +
  coord_flip() +
  facet_grid(Gen~.~YEAR) +
  scale_y_continuous(breaks=c(-3000000,-1500000,0,1500000,3000000),
                     labels=c('3','1.5','0','1.5','3')) +
  labs(y='Population in Millions',title='Population Pyramids for Immigrants and their Children') +
  scale_fill_brewer(palette='Set1',guide=guide_legend(reverse=TRUE))+
  guides(fill=guide_legend(title='Sex',title.position='top')) +
  theme_bw() + theme(legend.position='bottom') 
dev.off()

#To animate the pyramids:

#Run the following once:
install.packages('devtools')
library(devtools)
devtools::install_github('dgrtwo/gganimate')

#Load gganimate
library(gganimate)

#Read in data -- new dataset with 1880-1960 (all years);
#filter out Alaska, Hawaii, 3+ generation
ipums <- read_csv('data/10_6_all.csv') %>%
  filter(YEAR>=1960 | !(STATEFIP %in% c(2,15))) %>%
  filter(BPL>99 | MBPL>99 | FBPL>99)

#Produce data frame for graphing
all.data <- ipums %>% mutate(Sex=factor(SEX,labels=c('Male','Female'))) %>%
  mutate(Age=factor(ifelse(AGE>=80,8,floor(AGE/10)),labels=agecats)) %>%
  mutate(Gen=ifelse(BPL>99,1,2)) %>%
  mutate(Weight=ifelse(YEAR %in% c(1940,1950) & BPL<=99,SLWT,PERWT)) %>%
  group_by(Age,Sex,Gen,YEAR) %>% summarise(Number=sum(Weight)) %>%
  mutate(Number=ifelse(Sex=='Male',-1*Number,Number))

#Iterates through the graphing function for each generation
#Key changes for animation: geom_bar function includes "position='identity'";
#aes function includes "frame=YEAR"; no facetting; gg_animate function animates
#and exports to gif
for (gen in 1:2) {
  pyr.data <- all.data %>% filter(Gen==gen)
  pyr <- ggplot(data=pyr.data,aes(x=Age,y=Number,fill=Sex,frame=YEAR)) +
    geom_bar(data=filter(pyr.data,Sex=='Male'),stat='identity',position='identity') +
    geom_bar(data=filter(pyr.data,Sex=='Female'),stat='identity',position='identity') +
    scale_y_continuous(breaks=c(-2000000,-1000000,0,1000000,2000000),
                     labels=c('2,000,000','1,000,000','0','1,000,000','2,000,000')) +
    labs(fill='',y='Population',title=paste('Population Pyramids for Generation ',gen,',',sep='')) +
    scale_fill_brewer(palette='Set1',guide=guide_legend(reverse=TRUE)) +
    theme_bw() + theme(legend.position='bottom') +
    coord_flip()
  gg_animate(pyr,paste('g',gen,'_pyramid.gif',sep=''))
}


