library(tibble)
library(tidyr)
library(readr)
library(ggplot2)
library(Amelia)
library(tidyverse)
library(plotly)
library(mice)
library(dplyr)

df1<-read.csv("train.csv")
df2<-read.csv("test.csv")

colnames(df1)

df11<-select(df1, -SalePrice )

idf<-rbind(df11, df2)
str(idf)
colSums(is.na(idf))

#fixed na in Alley column 
idf$Alley<-idf$Alley %>% replace_na("no alley")

idf$MiscFeature
unique(idf$FireplaceQu )
idf$FireplaceQu 

unique(idf$Alley)

str(idf)
idf["no alley"]<- NA
idf["pave"]<- NA
idf["Grvl"]<- NA

str(idf)
lapply(idf$`no alley`, idf$pave, idf$Grvl, integer)

idf$Grvl<-as.integer(idf$Grvl)
idf$pave<-as.integer(idf$pave)
idf$`no alley`<-as.integer(idf$`no alley`)

str(idf)
# now add dummy variables to no alley, pave and grvl 


idf$Alley<- ifelse(idf$Alley=="Pave", 1,0)

idf$Alley<- ifelse(idf$Alley=="no alley", 1,0)
idf$Alley<- ifelse(idf$Alley=="no alley", 1,0)

#extract integer vectors

num_col<-select_if(idf, is.numeric)

str(num_col)

# now estimate LotFrontage missing data (imputation)
#we test differentml models to predict 

iidf<- select(num_col, -c(pave,Grvl,Alley,"no alley", Id) )
str(iidf)

apply(iidf, median, na.ram=FALSE)

imputed_median<-iidf %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))


# first step we sepearet data to train and test 
library(caTools)
sample<-sample.split(imputed_median$LotFrontage, SplitRatio = 0.7)
train<- subset(imputed_median, sample==TRUE )
test <-subset(imputed_median, sample==FALSE)

str(iidf)

colSums(is.na(iidf))

iidf %>%select(LotFrontage, MasVnrArea) %>% filter (!complete.cases(.)) %>% view()

#lets first check how na data is distributed 
filter (!complete.cases(.)) %>% view()
md.pattern(iidf)

iidf%>% select(LotFrontage) %>% na.omit() %>% 
  view()

iidf%>% select(LotFrontage, MasVnrArea ) %>% na.omit() %>%  drop_na( MasVnrArea ) %>%
  view()

iidf$LotFrontage

iidf%>% select(LotFrontage) %>% mutate(LotFrontage= replace_na(LotFrontage, "NA"))
%>%filter (!complete.cases(.)) %>% view()

mean(iidf$LotFrontage)
mean(iidf$LotFrontage, na.rm=TRUE)

rlang::last_error()

input=df1
input

summary(input)

str(input)
help(apply)

as.factor(input$Fireplaces, input$BsmtFullBath)

as.factor(input$Fireplaces)
lapply(input$GarageCars, input$KitchenAbvGr, is.factor)
sapply(input$GarageCars, input$KitchenAbvGr, is.factor)
input["GarageCars", "KitchenAbvGr" ]<-lapply(input["GarageCars", "KitchenAbvGr" ], factor)

str(input$GarageCars)

df1$LotFrontage[which(is.na(df1$LotFrontage))]= mean(df1$LotFrontage,na.rm = F)

df1$LotFrontage %>% View()

df1 %>% view()
df2$LotFrontage[which(is.na(df2$LotFrontage))]= mean(df2$LotFrontage,na.rm = F)
df2$LotFrontage%>% view()
sum(is.na(df2$LotFrontage))

# mice imputation 
str(df2)
colSums(is.na(df2))
dd<- subset(df2,  GarageType =="Attchd")

test1<-select (df2, MasVnrArea,GarageYrBlt )

str(test1)

colSums(is.na(test1)

colSums(is.na(test1))
        
summary(df2)


str(dd)
my_imp=mice(df2)

test$FullBath <- as.factor(test$FullBath)
str(test)
test$HalfBath <- as.factor (test$HalfBath)

impuu=mice(test1,m=5, method =c("pmm","pmm") ,maxit=20)

impuu$imp$MasVnrArea
summary(test1$MasVnrArea)

cleandataset=complete(impuu, 5)

ggplot(cleandataset, aes(cleandataset$))+
  geom_boxplot()



colSums(is.na(test1))
mice(test1)
help("mice")
my_impp= mice(test, m=4, method=c("log","log","pmm","pmm"), maxit = 20)

summary(test$HalfBath)

colSums(is.na(test))



my_impp$imp$BedroomAbvGr
