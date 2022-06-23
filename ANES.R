#load the libraries
library(haven)
anes_timeseries_2016 <- read_sav("anes_timeseries_2016.sav")
anes2016<-anes_timeseries_2016
library(data.table)
library(tidyr)
library(dplyr)
library(naniar)
#long way of removing values of missing data from set
anes2016<-anes2016 %>% replace_with_na(replace=list(V162243=c(999, 998, -9, -8, -7, -6, -5, -4, -2)))
anes2016<-anes2016 %>% replace_with_na(replace=list(V162244=c(999, 998, -9, -8, -7, -6, -5, -4, -2)))
anes2016<-anes2016 %>% replace_with_na(replace=list(V162245=c(999, 998, -9, -8, -7, -6, -5, -4, -2)))
anes2016<-anes2016 %>% replace_with_na(replace=list(V162246=c(999, 998, -9, -8, -7, -6, -5, -4, -2)))
anes2016<-anes2016 %>% replace_with_na(replace=list(V162207=c(999, 998, -9, -8, -7, -6, -5, -4, -2)))
anes2016<-anes2016 %>% replace_with_na(replace=list(V162208=c(999, 998, -9, -8, -7, -6, -5, -4, -2)))
anes2016<-anes2016 %>% replace_with_na(replace=list(V162209=c(999, 998, -9, -8, -7, -6, -5, -4, -2)))
anes2016<-anes2016 %>% replace_with_na(replace=list(V162210=c(999, 998, -9, -8, -7, -6, -5, -4, -2)))
anes2016<-anes2016 %>% replace_with_na(replace=list(V161267=c(999, 998, -9, -8, -7, -6, -5, -4, -2)))
anes2016<-anes2016 %>% replace_with_na(replace=list(V161310x=c(999, 998, -9, -8, -7, -6, -5, -4, -2)))

#identifying items of cpv
egal<-data.frame(anes2016$V162243, anes2016$V162244, anes2016$V162245, anes2016$V162246)
colnames(egal)<-c("egal_donecess","egal_worryless","egal_notbigprob","egal_fewerprobs")
trad<-data.frame(anes2016$V162207, anes2016$V162208, anes2016$V162209, anes2016$V162210)
colnames(trad)<-c("trad_adjmoral","trad_lifestyl","trad_tolerant","trad_moretard")
#both of above cateogries are an agree to disagree scale
#this one they are given options
anescpv<-data.frame(egal, trad)
#merging the 2 cpv into one table
anescpv$race<-anes2016$V161310x
anescpv$weight<-anes2016$V160102
table(anes2016$V161310x)/4270
table(anes2016$V161267)
mean(anes2016$V161267, na.rm=TRUE)
sd(anes2016$V161267, na.rm=TRUE)
table(anes2016$V161342)

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

#removing NAs from practice subset
colnames(anes2016)
anescpv<-anescpv[(!is.na(anescpv$egal_donecess)&!is.na(anescpv$egal_worryless)&!is.na(anescpv$egal_notbigprob)
                  &!is.na(anescpv$egal_fewerprobs)&!is.na(anescpv$trad_adjmoral)&!is.na(anescpv$trad_lifestyl)
                  &!is.na(anescpv$trad_tolerant)&!is.na(anescpv$trad_moretard)&!is.na(anescpv$race)),]
summary(anescpv)
#model for data from subset
library(lavaan)
model<-'Equality=~egal_donecess+egal_worryless+egal_notbigprob+egal_fewerprobs
        MoralTraditionalism=~trad_adjmoral+trad_lifestyl+trad_tolerant+trad_moretard'
MLMresults<-cfa(model=model,data=anescpv,group="race",sampling.weights="weight",estimator="MLM",std.lv=TRUE)
summary(MLMresults,fit.measures=TRUE,standardized=TRUE)
fitted(MLMresults)$cov
lavInspect(MLMresults,"cov.all")
