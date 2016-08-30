
write('hello world',file('./hello2.txt'))

#this is a comment

#creating variables
a <- 3
b4 <- 5
cat <- 7

#reassigning value
a <- 9

#mathematical functions
a+b4
cat/a
d <- cat-b4

#string variables
g <- 'history'
hi <- 'is fun'
twelve <- '12'
y <- 'she said, "hi"'

#quotes inside quotes
x <- "he said, 'bye'"

#character functions
paste(g,hi,sep=' ')

paste(g,'always',hi,sep=' ')

h <- strsplit(hi,' ')[[1]][1]
i <- strsplit(hi,' ')[[1]][2]

strsplit(hi,' ')

j <- c(g,h,i)
print(j)

j[1]
j[3]

k <- c(1,3,5,7,9)

m <- k*2
print(m)

m-k
m+k

2 < 3
3 < 2
3 == 2+1

paste(g,h,'not',i,sep=' ')

2 %in% k
print(k)

2 %in% m
print(m)

rm(list=ls())


#data frames
fname <- c('Ann','Bob','Carlos','Darlene','Eric','Fred','Gina','Harriet','Ivan','Jenna')
syear <- c(2000,1980,1991,1987,2003,1985,1996,2010,2007,1989)
byear <- c(1955,1960,1963,1948,1945,1951,1958,1967,1971,1960)
salary <- c(25000,50000,35000,35000,40000,30000,45000,25000,50000,30000)

employees <- data.frame(fname,syear,byear,salary)

View(employees)

names(employees)

head(employees)
tail(employees)

library(dplyr)

View(employees)

#sort data frame
employeesorted <- employees %>% arrange(byear)
head(employeesorted)

employees <- employees %>% arrange(byear)
head(employees)

employees %>% arrange(salary)
head(employees)

employees <- employees %>% arrange(salary,syear)
head(employees)

employees <- employees %>% arrange(-byear)
head(employees)

employees <- employees %>% arrange(salary,-byear)
head(employees)

#subset by columns
sub <- employees %>% select(fname,byear)
head(sub)

newsub <- employees %>% select(-fname,-byear)
head(newsub)

#subset by rows
sub <- employees %>% filter(byear<1960)
head(sub)

newsub <- employees %>% filter(byear<1960 & salary>30000)
head(newsub)
newnewsub <- employees %>% filter(byear<1960 | salary>=40000)
head(newnewsub)

#subset by rows and columns
sub <- employees %>% filter(syear<2000)
subsub <- sub %>% select(fname,byear)
head(subsub)

subsub <- employees %>% filter(syear<2000) %>% select(fname,byear)
head(subsub)

#create new variables
employees <- employees %>% mutate(status = 'employee')
head(employees)

employees2 <- employees %>% mutate(age=2016-byear)
head(employees2)

employees3 <- employees %>% mutate(age=2016-byear,service=2016-syear)
head(employees3)

pensions <- employees3 %>% mutate(pension=salary*age/100*service/100)
head(pensions)

pensions <- employees %>% mutate(pension=salary*(2016-byear)/100*(2016-syear)/100)
head(pensions)

pensions <- employees %>% mutate(pension=ifelse(2016-byear>=60,
                                                salary*(2016-byear)/100*(2016-syear)/100,0))
head(pensions)




