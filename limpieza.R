#install.packages("textrecipes")
# install.packages("stopwords")
library(tidyverse)
library(dplyr)
library(psych)
library(purrr)
library(tidyr)
library(textrecipes)

train <- read.csv("~/Documents/Semestre I/Minería de datos/train.csv")
train<-data.frame(train)
  Moda1 <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
#Convert Month to factor
train$Month<-match(train$Month,month.name)


#missing values de Monthly_Inhand_Salary, Name, Age
#characters that won't change
train<-train %>% group_by(Customer_ID) %>% mutate(Name=Moda1(Name)) 
train<-train %>% group_by(Customer_ID) %>% mutate(SSN=Moda1(SSN)) 
train<-train %>% group_by(Customer_ID) %>% fill(Occupation,.direction="updown") 
view(train)
#VARIABLES THAT COULD CHANGE
train<-train %>% group_by(Customer_ID) %>% fill(Age,.direction="updown") %>% view()
train<-train %>% group_by(Customer_ID) %>% fill(Monthly_Inhand_Salary,.direction="updown")
 #removing typos
train<-str_remove_all(train, "_") #removing _ typo

#check following cleaning: name, Customer_ID, Month
count(train$Name)
cuenta1<-train %>%group_by(Customer_ID) %>% count()
cuenta1<-sort(cuenta1,descending=F)


