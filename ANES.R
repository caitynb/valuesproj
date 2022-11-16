####set personal work environment
setwd("C:/Users/Caity/Downloads/McNair_Values")

####clean out R environment
rm(list=ls())

####import dataset and load libraries
library(haven)
library(data.table)
library(tidyr)
library(dplyr)
library(naniar)
library(Hmisc)
library(lavaan)
library(psy)
library(tidyverse)
library(xtable)
library(car)
library(summarytools)

anes_timeseries_2016 <- read_sav("anes_timeseries_2016.sav")
anes2016<-anes_timeseries_2016

####create new variables from original ANES variables before data cleaning
#moral traditionalism items
anes2016$moral2<-anes2016$V162207
anes2016$moral1<-anes2016$V162208
anes2016$moral3<-anes2016$V162209
anes2016$moral4<-anes2016$V162210

#race and weights
anes2016$race<-anes2016$V161310x
anes2016$weight<-anes2016$V160102

table(anes2016$race)

#remove negative values and replace with NA
anes2016$moral1<-ifelse(anes2016$moral1<0,NA,anes2016$moral1)
anes2016$moral2<-ifelse(anes2016$moral2<0,NA,anes2016$moral2)
anes2016$moral3<-ifelse(anes2016$moral3<0,NA,anes2016$moral3)
anes2016$moral4<-ifelse(anes2016$moral4<0,NA,anes2016$moral4)

#race needs special coding, the goal is to have Hispanic (originally identified by value 5)
anes2016$race<-ifelse(anes2016$race==3,NA,anes2016$race)
anes2016$race<-ifelse(anes2016$race==5,3,anes2016$race)
anes2016$race<-ifelse(anes2016$race<0|anes2016$race>3,NA,anes2016$race)

#recode items to make them reverse coded, so a 5 indicates a stronger 
# level of traditionalism and a 1 is a weak level of traditionalism
anes2016$moral1<-recode(anes2016$moral1,"1=5;2=4;3=3;4=2;5=1")
anes2016$moral4<-recode(anes2016$moral4,"1=5;2=4;3=3;4=2;5=1")

#next step of cleaning that makes the correlations easy is to subset the factors
# of interest (the moral traditionalism items and race)
aneswhole<-subset(anes2016,select=c(moral1,moral2,moral3,moral4,race))
aneswhole<-aneswhole[(!is.na(aneswhole$moral1)&!is.na(aneswhole$moral2)
                   &!is.na(aneswhole$moral3)&!is.na(aneswhole$moral4)),]

########create correlation matrices
#whole population
cor(aneswholen)
rcorr(as.matrix(aneswholen[,1:4]))
cor(subset(aneswholen$race==1))

#white sample
aneswhite<-subset(aneswholen, race==1)
cor(aneswhite[,1:4])
rcorr(as.matrix(aneswhite[,1:4]))  

#black sample
anesblack<-subset(aneswholen, race==2)
cor(anesblack[,1:4])
rcorr(as.matrix(anesblack[,1:4]))

#hispanic sample
aneshisp<-subset(aneswholen,race==3)
cor(aneshisp[,1:4])
rcorr(as.matrix(aneshisp[,1:4]))



ggplot(data=datt, aes(x=sample, y=ravg, fill=value)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=ravg), vjust=1.5, color="black",
            position = position_dodge(0.9), size=5)+
  theme_minimal()+ylab(label="Average Correlation")+xlab(label="Race of Sample")+scale_fill_brewer(palette="Blues")+theme(legend.position="top",legend.title=element_blank())+ggtitle("Core Political Values")

