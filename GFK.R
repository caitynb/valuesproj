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
RaceGFK <- read_dta("/Users/Caity/Downloads/RaceGFK.dta")
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

univ<-data.frame(RaceGFK$univ1,RaceGFK$univ2,RaceGFK$univ3,RaceGFK$univ4)
cons<-data.frame(RaceGFK$cons1,RaceGFK$cons2,RaceGFK$cons3,RaceGFK$cons4)
#remove NA
RaceGFK<-RaceGFK[(!is.na(RaceGFK$univ1)&!is.na(RaceGFK$univ2)&!is.na(RaceGFK$univ3)
                  &!is.na(RaceGFK$univ4)&!is.na(RaceGFK$cons1)&!is.na(RaceGFK$cons2)
                  &!is.na(RaceGFK$cons3)&!is.na(RaceGFK$cons4)),]
#cfa
colnames(RaceGFK)
model<-'Universalism=~univ1+univ2+univ3+univ4
        Conservation=~cons1+cons2+cons3+cons4'
MLMresults<-cfa(model,data=RaceGFK,group="race",estimator="MLM")
summary(MLMresults,fit.measures=TRUE,standardized=TRUE)
table(RaceGFK$race)
fitmeasures(MLMresults)
library(semPlot)
semPaths(MLMresults,what="std", nCharNodes=1, rotation=2,intercepts=FALSE)
library(Hmisc)
#correlation of entire sample
dat<-rcorr(as.matrix(RaceGFK[,1:4]),type="pearson")
dat
dat2<-rcorr(as.matrix(RaceGFK[,5:8]),type="pearson")
dat2
mean(dat2$r)
#subset of race
race1<-subset(RaceGFK, subset=race==1)
race2<-subset(RaceGFK,subset=race==2)
race3<-subset(RaceGFK,subset=race==3)
#correlation of univ by race
datr1u<-rcorr(as.matrix(race1[,1:4]),type="pearson")
datr1u
mean(datr1u$r)
datr2u<-rcorr(as.matrix(race2[,1:4]),type="pearson")
datr2u
mean(datr2u$r)
datr3u<-rcorr(as.matrix(race3[,1:4]),type="pearson")
datr3u
mean(datr3u$r)
#correlation of cons by race
datr1c<-rcorr(as.matrix(race1[,5:8]),type="pearson")
mean(datr1c$r)
datr2c<-rcorr(as.matrix(race2[,5:8]),type="pearson")
mean(datr2c$r)
datr3c<-rcorr(as.matrix(race3[,5:8]),type="pearson")
mean(datr3c$r)

#make scatterplot
colnames(univ)<-c("Item 1", "Item 2", "Item 3", "Item 4")
install.packages("corrplot")
library(corrplot)
corrplot(dat$r, type="lower", tl.col="black")
corrplot(dat$r, type="lower",method="shade", 
         addCoef.col="black",tl.pos="n",p.mat=dat$p, sig.level=0.05)
