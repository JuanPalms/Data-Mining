
---
title: "limpieza1"
output: html_document
date: "2022-09-09"
---

```{r setup, include=FALSE}

```

Instalar paquetes necesarios

```{r}
#install.packages("textrecipes")
# install.packages("stopwords")
#install.packages("EmpiricalCalibration")
library(tidyverse)
library(dplyr)
library(psych)
library(purrr)
library(tidyr)
library(textrecipes)
library(EmpiricalCalibration)
library(base)
```
importar datos y función de moda para variables string
```{r}
train <- read.csv("~/Documents/Semestre I/Mineria de datos/train.csv")
#functions for character modes and typos
  Moda1 <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  Typo1<-function(x){
    x<-as.numeric(x)
    t<-length(x)
    for(i in 2:t) {
      x[i]<-ifelse(abs(x[i]-x[i-1])<2,x[i],x[i-1])
    }
    return(x)
  }
```


```{r}
 #removing typos

#string variables
  train1<-train %>% group_by(Customer_ID) %>% fill(Age,.direction="updown") %>% mutate(Name=Moda1(Name)) %>% mutate(Occupation=Moda1(Occupation)) %>% mutate(SSN=Moda1(SSN)) 


#numeric variables
#1.removing typos pt1 (mostly "_" among others)

train1<-train1 %>% group_by(Customer_ID) %>%mutate(Age=gsub("_","",Age)) %>% mutate(Annual_Income=gsub("_","",Annual_Income)) %>% mutate(Num_of_Loan=gsub("_","",Num_of_Loan)) %>% mutate(Changed_Credit_Limit=gsub("_","",Changed_Credit_Limit)) %>% mutate(Credit_Mix=gsub("_","",Credit_Mix)) %>% mutate(Outstanding_Debt=gsub("_","",Outstanding_Debt)) %>% mutate(Amount_invested_monthly=gsub("_","",Amount_invested_monthly)) %>% mutate(Num_of_Delayed_Payment=gsub("_","",Num_of_Delayed_Payment)) %>% mutate(Monthly_Balance=gsub("_","",Monthly_Balance)) %>% mutate(Amount_invested_monthly=gsub("!@9#%8","",Amount_invested_monthly)) %>% mutate(Credit_Mix=gsub("_","",Credit_Mix))  

#2.filling missing values

train1<-train1%>% fill(Age,.direction="updown") %>% fill(Monthly_Inhand_Salary,.direction="updown") %>%fill(Annual_Income,.direction="updown") %>%  fill(Num_Bank_Accounts,.direction="updown") %>% fill(Type_of_Loan,.direction="updown") %>% fill(Num_of_Delayed_Payment,.direction="updown") %>% fill(Num_Credit_Inquiries,.direction="updown") %>% fill(Credit_History_Age,.direction="updown") %>% fill(Credit_Mix,.direction="updown") %>% fill(Changed_Credit_Limit,.direction="updown") %>% fill(Amount_invested_monthly,.direction="updown")

#as you can see there aren't missing values anymore. Up to this point no outliers were removed tho
describe(train1)
  
```

```{r}
train1<-train1 %>%  %>% fill(Annual_Income,.direction="updown") %>% fill(Num_of_Delayed_Payment,.direction="updown") %>% fill(Changed_Credit_Limit,.direction="updown") %>% fill(Outstanding_Debt,.direction="updown") %>% fill(Amount_invested_monthly,.direction="updown") 
  
  train1<-train1 %>% group_by(Customer_ID) %>% fill(Type_of_Loan,.direction="updown") #tuve que ponerla al principio porque hay chunks de NA no especificado
#train1[train1==""] <- NA
 train1$Type_of_Loan[train1$Type_of_Loan==""]<-"Not Specified"

 print(train1)
```

Limpiar variables que no cambian

```{r}
#removing missing values


train1<-train1 %>% group_by(Customer_ID)%>%
  
#Month	
  mutate(Month=match(Month,month.name) ) %>%
#Name	
   mutate(Name=Moda1(Name)) %>% 
#Age	
   mutate(Age=str_remove_all(Age,"_")) %>% as.numeric(Age) %>%
  fill(Age,.direction="updown") %>%
  Typo1(Age) %>%
  
#SSN
  mutate(SSN=Moda1(SSN)) %>%
#Occupation	
  mutate(Occupation=Moda1(Occupation)) %>%
  fill(Occupation,.direction="updown") %>%
#Annual_Income	
   mutate(Annual_Income=str_remove_all(Annual_Income,"_"))%>%
#Monthly_Inhand_Salary
fill(Monthly_Inhand_Salary,.direction="updown") %>%
#Num_Bank_Accounts	
#Num_Credit_Card	
#Interest_Rate	
#Num_of_Loan	
#Type_of_Loan	
    
#Delay_from_due_date	
#Num_of_Delayed_Payment
 mutate(Num_of_Delayed_Payment=str_remove_all(Num_of_Delayed_Payment,"_")) %>%
  fill(Num_of_Delayed_Payment,.direction="updown") %>% #recent
#Changed_Credit_Limit	
 mutate(Changed_Credit_Limit=str_remove_all(Changed_Credit_Limit,"_")) %>%
#Num_Credit_Inquiries	
#Credit_Mix	
mutate(Credit_Mix=str_remove_all(Credit_Mix,"_")) 
#Outstanding_Debt	
#Credit_Utilization_Ratio	
#Credit_History_Age	
#Payment_of_Min_Amount	
#Total_EMI_per_month	
#Amount_invested_monthly	
#Payment_Behaviour	
#Monthly_Balance	
#Credit_Score

#view(train1)
```

Limpiar variables que pueden cambiar

```{r}
#VARIABLES THAT COULD CHANGE



#check following cleaning: name, Customer_ID, Month
count(train$Name)
cuenta1<-train %>%group_by(Customer_ID) %>% count()
cuenta1<-sort(cuenta1,descending=F)
```


## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
