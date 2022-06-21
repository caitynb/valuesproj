#load libraries
library(haven)
library(tidyr)
library(dplyr)
library(naniar)
library(lavaan)
library(psy)
library(tidyverse)
library(xtable)
#upload data
RaceGFK <- read_dta("C:/Users/Caity/Downloads/RaceGFK.dta")
#make all numeric
class(RaceGFK$univ1)
RaceGFK$univ1<-as.numeric(RaceGFK$univ1)
class(RaceGFK$univ2)
RaceGFK$univ2<-as.numeric(RaceGFK$univ2)
class(RaceGFK$univ3)
RaceGFK$univ3<-as.numeric(RaceGFK$univ3)
class(RaceGFK$univ4)
RaceGFK$univ4<-as.numeric(RaceGFK$univ4)
class(RaceGFK$cons1)
RaceGFK$cons1<-as.numeric(RaceGFK$cons1)
class(RaceGFK$cons2)
RaceGFK$cons2<-as.numeric(RaceGFK$cons2)
class(RaceGFK$cons3)
RaceGFK$cons3<-as.numeric(RaceGFK$cons3)
class(RaceGFK$cons4)
RaceGFK$cons4<-as.numeric(RaceGFK$cons4)
class(RaceGFK$race)
RaceGFK$race<-as.numeric(RaceGFK$race)

#remove NA
RaceGFK<-RaceGFK[(!is.na(RaceGFK$univ1)&!is.na(RaceGFK$univ2)&!is.na(RaceGFK$univ3)
                    &!is.na(RaceGFK$univ4)&!is.na(RaceGFK$cons1)&!is.na(RaceGFK$cons2)
                    &!is.na(RaceGFK$cons3)&!is.na(RaceGFK$cons4)&!is.na(RaceGFK$race)),]
#cfa
colnames(RaceGFK)
model<-'Universalism=~univ1+univ2+univ3+univ4
        Conservation=~cons1+cons2+cons3+cons4'
MLMresults<-cfa(model,data=RaceGFK,group="race",estimator="MLM")
summary(MLMresults,fit.measures=TRUE,standardized=TRUE)
