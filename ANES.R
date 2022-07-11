#load the libraries
#import dataset and load libraries
library(haven)
anes_timeseries_2016 <- read_sav("anes_timeseries_2016.sav")
anes2016<-anes_timeseries_2016
library(data.table)
library(tidyr)
library(dplyr)
library(naniar)
library(Hmisc)
#egal dataframe
egal<-data.frame(anes2016$V162243, anes2016$V162244, anes2016$V162245, anes2016$V162246)
#trad dataframe
trad<-data.frame(anes2016$V162207, anes2016$V162208, anes2016$V162209, anes2016$V162210)
#combine in their own
anescpv<-cbind.data.frame(egal, trad)
#add race and weight column
anescpv$race<-anes2016$V161310x
anescpv$weight<-anes2016$V160102
#replace missing data points with na (-2, -9, etc)
summary(anescpv)
anescpv[anescpv<=0] <- NA
#rename column names 
colnames(anescpv)<-c("V162243","V162244","V162245","V162246","V162207",
                     "V162208","V162209","V162210", "race","weight")
#omit NAs
anescpv<-anescpv[(!is.na(anescpv$V162243)&!is.na(anescpv$V162244)
                  &!is.na(anescpv$V162245)
                  &!is.na(anescpv$V162246)&!is.na(anescpv$V162207)
                  &!is.na(anescpv$V162208)
                  &!is.na(anescpv$V162209)&!is.na(anescpv$V162210)),]

#subset for each race
aneswhite<-subset(anescpv, race==1)
anesblack<-subset(anescpv, race==2)
aneshisp<-subset(anescpv,race==5)


#whole sample
wholetrad<-rcorr(as.matrix(anescpv[,5:8]),type="pearson")
wholeegal<-rcorr(as.matrix(anescpv[,1:4]),type="pearson")
mean(abs(wholetrad))
mean(abs(wholeegal$r))
wholetrad<-cor(anescpv[,5:8])
#white sample
whitetrad<-rcorr(as.matrix(aneswhite[,5:8]),type="pearson")
whiteegal<-rcorr(as.matrix(aneswhite[,1:4]),type="pearson")
mean(whitetrad$r)
mean(abs(whitetrad$r))
mean(whiteegal$r)
mean(abs(whiteegal$r))
#black sample
blacktrad<-rcorr(as.matrix(anesblack[,5:8]),type="pearson")
blackegal<-rcorr(as.matrix(anesblack[,1:4]),type="pearson")
mean(blacktrad$r)
mean(abs(blacktrad$r))
mean(blackegal$r)
mean(abs(blackegal$r))
#hispanic sample
hisptrad<-rcorr(as.matrix(aneshisp[,5:8]),type="pearson")
hispegal<-rcorr(as.matrix(aneshisp[,1:4]),type="pearson")
mean(hisptrad$r)
mean(abs(hisptrad$r))
mean(hispegal$r)
mean(abs(hispegal$r))

#change all columns in subset to numeric
class(anescpv$egal_donecess)
 anescpv$egal_donecess<-as.numeric(anescpv$egal_donecess)
class(anescpv$egal_worryless)
anescpv$egal_worryless<-as.numeric(anescpv$egal_worryless)
class(anescpv$egal_notbigprob)
anescpv$egal_notbigprob<-as.numeric(anescpv$egal_notbigprob)
class(anescpv$egal_fewerprobs)
anescpv$egal_fewerprobs<-as.numeric(anescpv$egal_fewerprobs)
class(anescpv$trad_adjmoral)
anescpv$trad_adjmoral<-as.numeric(anescpv$trad_adjmoral)
class(anescpv$trad_lifestyl)
anescpv$trad_lifestyl<-as.numeric(anescpv$trad_lifestyl)
class(anescpv$trad_tolerant)
anescpv$trad_tolerant<-as.numeric(anescpv$trad_tolerant)
class(anescpv$trad_moretard)
anescpv$trad_moretard<-as.numeric(anescpv$trad_moretard)
class(anescpv$race)
anescpv$race<-as.character(anescpv$race)
class(anescpv$weight)
egal<-egal
#removing NAs from practice subset
colnames(anes2016)
anescpv<-anescpv[(!is.na(anescpv$egal_donecess)&!is.na(anescpv$egal_worryless)&!
                    is.na(anescpv$egal_notbigprob)
                  &!is.na(anescpv$egal_fewerprobs)&!is.na(anescpv$trad_adjmoral)
                  &!is.na(anescpv$trad_lifestyl)
                  &!is.na(anescpv$trad_tolerant)&!is.na(anescpv$trad_moretard)&!
                    is.na(anescpv$race)),]
summary(anescpv)
#model for data from subset
library(lavaan)
model<-'Equality=~egal_donecess+egal_worryless+egal_notbigprob+egal_fewerprobs
        MoralTraditionalism=~trad_adjmoral+trad_lifestyl+trad_tolerant+trad_moretard'
MLMresults<-cfa(model=model,data=anescpv,group="race",sampling.weights="weight",
                estimator="MLM",std.lv=TRUE)
summary(MLMresults,fit.measures=TRUE,standardized=TRUE)
fitted(MLMresults)$cov
lavInspect(MLMresults,"cov.all")


