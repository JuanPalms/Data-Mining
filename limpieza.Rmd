
---
title: "limpieza1"
output: html_document
date: "2022-09-09"
---

Instalar paquetes necesarios

```{r}
#NOTA: instalar estos paquetes antes de correr el código
#install.packages("textrecipes")
# install.packages("stopwords")
#install.packages("mltools")
library(tidyverse)
library(dplyr)
library(psych)
library(purrr)
library(tidyr)
library(textrecipes)
library(base)
```
importar datos y función de moda para variables string
```{r}
train <- read.csv("~/Documents/Semestre I/Mineria de datos/train.csv")
```

```{r}
#functions for character modes and typos
  Moda1 <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

```


```{r}
 # removing typos


#removing missing values and typos from string variables that may not change
  train1<-train %>% group_by(Customer_ID) %>% fill(Age,.direction="updown") %>% mutate(Name=Moda1(Name)) %>% mutate(Occupation=Moda1(Occupation)) %>% mutate(SSN=Moda1(SSN)) %>% mutate(Type_of_Loan=Moda1(Type_of_Loan)) %>% mutate(Month=match(Month,month.name))

#special case:type of loan
 train1$Type_of_Loan[train1$Type_of_Loan==""]<-"Not Specified"


#numeric variables
#removing typos pt1 (mostly "_" among others)

train1<-train1 %>% group_by(Customer_ID) %>% mutate(Age=gsub("_","",Age)) %>% mutate(Occupation=gsub("_","",Occupation)) %>% mutate(Annual_Income=gsub("_","",Annual_Income)) %>% mutate(Num_of_Loan=gsub("_","",Num_of_Loan)) %>% mutate(Changed_Credit_Limit=gsub("_","",Changed_Credit_Limit)) %>% mutate(Changed_Credit_Limit=gsub(" ","",Changed_Credit_Limit)) %>% mutate(Credit_Mix=gsub("_","",Credit_Mix)) %>% mutate(Credit_Mix=gsub(" ","",Credit_Mix)) %>% mutate(Outstanding_Debt=gsub("_","",Outstanding_Debt)) %>% mutate(Amount_invested_monthly=gsub("_","",Amount_invested_monthly)) %>% mutate(Num_of_Delayed_Payment=gsub("_","",Num_of_Delayed_Payment)) %>% mutate(Monthly_Balance=gsub("_","",Monthly_Balance)) %>% mutate(Amount_invested_monthly=gsub("!@9#%8","",Amount_invested_monthly)) %>% mutate(Payment_Behaviour=gsub("!@9#%8","",Payment_Behaviour)) 

```


```{r}

  #outlier removal process

#los valores se encontraron con la formula quantile en una primera iteración. Dado que quantile no admite vectores con datos faltantes, el algoritmo de quitar los outliers sin sentido se hizo a mano en una segunda iteración. El algoritmo funciona de la siguiente manera: Si los cuantiles 2% o 98% eran muy despegados a la media, el valor de la celda se cambia por NA

#Age    
train1$Age<-as.numeric(train1$Age)
    train1$Age[train1$Age<10]=NA
    train1$Age[train1$Age>60]=NA
    
#Annual income
    train1$Annual_Income<-as.numeric(train1$Annual_Income)
    train1$Annual_Income[train1$Annual_Income>200000]=NA

    #bANK ACCOUNTS
    train1$Num_Bank_Accounts<-as.numeric(train1$Num_Bank_Accounts)
     train1$Num_Bank_Accounts[train1$Num_Bank_Accounts>20]=NA
     train1$Num_Bank_Accounts[train1$Num_Bank_Accounts<0]=NA
     
   #Credit cards
    train1$Num_Credit_Card<-as.numeric(train1$Num_Credit_Card)
    train1$Num_Credit_Card[train1$Num_Credit_Card>20]=NA
  
       #Interest rate
    train1$Interest_Rate<-as.numeric(train1$Interest_Rate)
    train1$Interest_Rate[train1$Interest_Rate>129]=NA
    
  #Number of loans
    train1$Num_of_Loan<-as.numeric(train1$Num_of_Loan)
     train1$Num_of_Loan[train1$Num_of_Loan<0]=NA
     train1$Num_of_Loan[train1$Num_of_Loan>20]=NA
     
    #Delayed payment
    train1$Num_of_Delayed_Payment<-as.numeric(train1$Num_of_Delayed_Payment)
    train1$Num_of_Delayed_Payment[train1$Num_of_Delayed_Payment<0]=NA
    train1$Num_of_Delayed_Payment[train1$Num_of_Delayed_Payment>40]=NA
    
        #Num credit inquiries
     
    train1$Num_Credit_Inquiries<-as.numeric(train1$Num_Credit_Inquiries)
    train1$Num_Credit_Inquiries[train1$Num_Credit_Inquiries>20]=NA
    
        #EMI per month
     
    train1$Total_EMI_per_month<-as.numeric(train1$Total_EMI_per_month)
    train1$Total_EMI_per_month[train1$Total_EMI_per_month>1000]=NA
    train1$Total_EMI_per_month[train1$Total_EMI_per_month==""]=NA
    
     #Changed credit limit
     
    train1$Changed_Credit_Limit<-as.numeric(train1$Changed_Credit_Limit)
    train1$Changed_Credit_Limit[train1$Changed_Credit_Limit==""]=NA
    
    #Monthly balance
     
    train1$Monthly_Balance<-as.numeric(train1$Monthly_Balance)
    train1$Monthly_Balance[train1$Monthly_Balance==""]=NA
    
    #string variables: Credit mix, payment behaviour, payment min amount
     
    train1$Credit_Mix[train1$Credit_Mix==""]=NA
    train1$Payment_Behaviour[train1$Payment_Behaviour==""]=NA
    train1$Payment_of_Min_Amount[train1$Payment_of_Min_Amount=="NM"]=NA
    
     #Amount invested
     
    train1$Amount_invested_monthly<-as.numeric(train1$Amount_invested_monthly)
    train1$Amount_invested_monthly[train1$Amount_invested_monthly==""]=NA
    train1$Amount_invested_monthly[train1$Amount_invested_monthly>5000]=NA



```


```{r}
#filling missing values


train1<-train1%>% group_by(Customer_ID) %>% fill(Age,.direction="updown") %>% fill(Monthly_Inhand_Salary,.direction="updown") %>%fill(Annual_Income,.direction="updown") %>%  fill(Num_Bank_Accounts,.direction="updown")  %>% fill(Num_Credit_Card,.direction="updown")  %>% fill(Num_of_Delayed_Payment,.direction="updown") %>% fill(Num_Credit_Inquiries,.direction="updown") %>%  fill(Credit_History_Age,.direction="updown") %>% fill(Credit_Mix,.direction="updown") %>% fill(Changed_Credit_Limit,.direction="updown") %>% fill(Amount_invested_monthly,.direction="updown")  %>% fill(Num_of_Loan,.direction="updown")  %>% fill(Interest_Rate,.direction="updown")  %>% fill(Payment_of_Min_Amount,.direction="updown")  %>% fill(Total_EMI_per_month,.direction="updown")  %>% fill(Payment_Behaviour,.direction="updown") %>%  fill(Type_of_Loan,.direction="updown") %>% fill(Monthly_Balance,.direction="updown")
  
```

base de datos final sin variables repetidas

```{r}
train2<-train1 %>% select(-ID,-Name,-SSN,-Credit_History_Age)

```


exportar a csv

```{r}
write.csv(train2,file="limpieza_credito.csv",row.names=FALSE)
```

