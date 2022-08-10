#cleaning housing data _imputing using SVM estimate due to high heteroscedasticity

df<-read.csv("train.csv")
df1<-read.csv('test.csv')

#drop target variable
dff<-select(df, -SalePrice )
alldata<-rbind(dff, df1)
str(alldata)

#explore data EDA
head(df)
str(df[1:10])
library(Amelia)
missmap(df)
is.null(df)
missmap(df[1])

colSums(is.na(df))

head(df$Fence)
missmap(df)
colSums(is.na(df))
rowSums(is.na(df))
#columons have significant number of missing values 

df[!complete.cases(df),]

df$Alley

library(dplyr)
is.na(df)

max(colSums(is.na(df)))

colSums(is.na(df))    

# having lots of zero in column makes problem - if there is limiated number of rows with sero we may filling them with mean, but if not we should drop 
# having nulls in column make problem 

#preprocessing categorical data 

#how we can extratc datatype which are categorical?

df<-select(df,-Id, )

colSums(is.na(df$RoofMatl))
colSums(is.na(df$LotArea))
summary(df)
install.packages("tidyverse")
library(ggplot2)
library (dplyr)
library(tidyr)
library(readr)
library(purrr)
library (tibble)
library(stringr)
library(forcats)
install.packages("purr")

df.complete <-na.omit(df)
colSums(is.na(df.complete))

str(df.complete)

glimpse(df.complete)

glimpse(df)
str(df)
typeof(df$Street)

unique(df$Street)
colnames(df)


file.info("train.csv")$size

install.packages("DataExplorer")
library(DataExplorer)
str(df)
unique(df$BsmtFullBath  )
unique(df$HalfBath)
is.matrix(df)
is.data.frame(df)

plot_str(df$Foundation)
plot_histogram(df$LotFrontage)
plot_missing(df[75:81])
#remove list 
#alley #Grage type  # garageyrvbit #poolqc #miscfeature #fence 
# drop columns with large missing values 

dropped_df<-select(df, -Fence, - MiscFeature, -PoolQC, -GarageYrBlt, -GarageType, - Alley )
str(dropped_df)

df$Fireplaces<- as.factor(df$Fireplaces)



dropped_df<-select(df, -Fence, - MiscFeature, -PoolQC, -GarageYrBlt, -GarageType, - Alley )
str(dropped_df)
colSums(is.na(dropped_df))
#we have null, how can we imputing na value 
typeof(df$FireplaceQu)

unique(dropped_df$ LotFrontage    )

str(dropped_df)
# designing for loop to read datatype and
head(df$b)
#factor 

colnames(dropped_df)
colSums(is.na(dropped_df))
#recycling colnames 

#null in numeric data 

cols_only(is.factor(dropped_df))

numeric.df<- select_if(dropped_df, is.numeric)
factor.df<-select_if(dropped_df, is.factor)

str(numeric.df)
str(factor.df)

dfcombined<-cbind.data.frame(numeric.df,factor.df )
str(dfcombined)

#now we have numeric data iclude factor and integer - lets see how to impute na

colSums(is.na(dfcombined))
# LotFrontage  and MasVnrArea with na
#lets see EDA to better understand these two data 


summary(dfcombined$LotFrontage)

colSums(is.na(dfcombined))
str(dfcombined)

plot_correlation(dfcombined[1:30])






      





            

  

