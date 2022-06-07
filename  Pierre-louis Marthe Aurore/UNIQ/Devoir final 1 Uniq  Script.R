####### Importation Data #################### 

library(tidyverse)
library(readxl)
library(tseries)
library(lmtest)
library(dplyr)
library(ggplot2)
library(aTSA)
library(dataseries)

url("https://www.brh.ht/wp-content/uploads/agregatsmon.xls")

setwd(file.path("/Applications/Devoir Marthe Manipulation"))
local<-file.path("Brh_Data.xls")
download.file("https://www.brh.ht/wp-content/uploads/agregatsmon.xls",local)
Brh_Data<-read_excel("Brh_Data.xls")


#Creer un objet comportant tous les colonnes de donnees du fichier de octobre 1990 a dat
Brh_Data<-Brh_Data[-c(4:146), ]

# Prendre une ligne approprie comme nom pour les colonnes
colnames(Brh_Data)<-Brh_Data[2,]


# changer le nom da la 1ere colonne en date
colnames(Brh_Data)...Brh_Data, Date =  C(1)) 


#Eliminer les colonnes et les lignes inutiles
Brh_Data<-Brh_Data[-c(531:1353), ]
Brh_Data<-Brh_Data[-c(521:530), ]
Brh_Data<-Brh_Data[-c(1:3), ]
Brh_Data<-Brh_Data[ ,-c( 5) ]
Brh_Data<-Brh_Data[,-c(8)]
Brh_Data<-Brh_Data[,-c(9)]
Brh_Data<-Brh_Data[,-c(12)]
Brh_Data<-Brh_Data[,-c(16)]
Brh_Data<-Brh_Data[,-c(18)]
Brh_Data<-Brh_Data[,-c(19)]
Brh_Data<-Brh_Data[,-c(23)]
Brh_Data<-Brh_Data[,-c(27)]
Brh_Data<-Brh_Data[,-c(38)]
Brh_Data<-Brh_Data[,-c(46)]
Brh_Data<-Brh_Data[,-c(33)]
Brh_variable<-Brh_variable[-c(1:22), ]


#CHoix des variables
Brh_variable<-Brh_Data%>%
  select("Multiplicateur(M3/B)","Multiplicateur(M2/B)","Multiplicateur(M1/B)")

str(Brh_variable)

# convertir en donnee numeri
Brh_variable$`Multiplicateur(M3/B)`<-as.numeric(Brh_variable$`Multiplicateur(M3/B)`)
Brh_variable$`Multiplicateur(M2/B)`<-as.numeric(Brh_variable$`Multiplicateur(M2/B)`)
Brh_variable$`Multiplicateur(M1/B)`<-as.numeric(Brh_variable$`Multiplicateur(M1/B)`)
      
# changer le nom des colonnes
names(Brh_variable)<-c("MM3", "MM2", "MM1")

# Teste de stationarite pour les variables
ggplot(agregatsmon.data3, aes(x = Date, y = M3))+
  geom_line()+
  labs(title = " Aggregat monetaire M3",
       subtitle = "Periode: Octobre 1990 - Octobre 2021")

adf.test(Brh_variable$MM3)


ggplot(agregatsmon.data3, aes(x = Date, y = taux_change))+
  geom_line()+
  labs(title = " Le taux de change",
       y = "Taux de change",
       subtitle = "Periode: Octobre 1990 - Octobre 2021")

adf.test(Brh_variable$MM2)


ggplot(agregatsmon.data3, aes(x = Date, y = reserves_depot))+
  geom_line()+
  labs(title = " Les réserves nettes de changes BRH",
       y = "Reserves nettes avec depots des BCMs",
       subtitle = "Periode: Octobre 1990 - Octobre 2021")

adf.test(Brh_variable$MM1)


a <- diff(Brh_variable$MM3)
plot(a, ylab = "MM3")
adf.test(a, k=2)

b <- diff(Brh_variable$MM2)
plot(b, ylab = "MM2")
adf.test(b, k=2)

c <- diff(Brh_variable$MM1)
plot(c, ylab = "MM1")
adf.test(c, k=2)

# Test de la causalite de Granger
Testcausalite_1<- grangertest(MM3 ~ MM2, data = Brh_variable, order = 1)
Testcausalite_2<- grangertest(MM3 ~ MM1, data = Brh_variable, order = 2)


# Regression lineaire sur la teste de la causalite de granger       
library(readr)
reg_testcausalite<-lm( Testcausalite_1~Testcausalite_2, data = Brh_variable)
summary(reg_brh_variable)

