#handling missing data  (tidyverse, mice)

df<- read.csv( '/Users/aynaz/Downloads/FlowerPicks.csv')
View(x = df)

summary(df)
library(dplyr)
glimpse(df)

completecases<- complete.cases(df)== TRUE
completecases

boxplot(Score~ Time, data = df)

lm(formula =Score~ Time,data = df )


library(zoo)
x=na.locf(df)
summary(x)

library(mice)

md.pattern(df)

mymice<-mice(df, m=10, method="rf")

class(mymice)






mymice$imp$Score

complete(mymice, 5)

lmfit<-with(mymice, lm(Score~Time))
summary(pool(lmfit))

complete

#(extreme Studentized deviate) ESD

na.fill(df$Score, 5)

na.locf()
help("na.locf")

#detecting outlier 

install.packages("mvoutlier")

library(mvoutlier)

exterme studenerization methid
mean-t*SD
MEAN+T*SD

X=c(rnorm(10), 150)
t=3
m=mean(X)
s=sd(X)

b1= m-t*s
b2= m+t*s

y=ifelse(X>=b1 & X<=b2,0,1)

plot(X)
boxplot(X)

library(outliers)
dixon.test(X)
grubbs.test(X, type = 11)

#detecting outliers in multivariate data

library(mvoutlier)
head(df)
as.data.frame(df)

myout=sign1(df[,4:5], qqcrit=0.975)

