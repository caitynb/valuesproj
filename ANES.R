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
wholecor<-cor(aneswhole[,1:4])
lowerwhole<-wholecor[lower.tri(wholecor)]
wh<-round(mean(lowerwhole), digits=2)

#white sample
aneswhite<-subset(aneswhole, race==1)
whitecor<-cor(aneswhite[,1:4])
lowerwhite<-whitecor[lower.tri(whitecor)]
wi<-round(mean(lowerwhite), digits=2)

#black sample
anesblack<-subset(aneswhole, race==2)
blackcor<-cor(anesblack[,1:4])
lowerblack<-blackcor[lower.tri(blackcor)]
b<-round(mean(lowerblack), digits=2)

#hispanic sample
aneshisp<-subset(aneswhole,race==3)
hispcor<-cor(aneshisp[,1:4])
lowerhisp<-hispcor[lower.tri(hispcor)]
h<-round(mean(lowerhisp), digits=2)



###################correlation figure
#create dataframe for figure
average<-c(wh, wi, b, h)
race<-c("Whole","White","Black","Hispanic")
dat<-cbind.data.frame(average,race)

#relevel to ensure ordering on ggplot matches
dat$race<-factor(dat$race, levels=c("Whole","White","Black","Hispanic"))

#ggplot coding
anesplot<-ggplot(dat, aes(x=race, y=average,fill=race))+geom_bar(stat="identity")+theme_classic()+
  geom_text(aes(label=average), vjust=1.5, color="black",position = position_dodge(0.9), size=4)
anesplot<-anesplot + labs(title="Average Correlation for Moral Traditionalism \nby Race (Figure 1)",subtitle="",y="Average Correlation",x="Race")+
  theme(legend.position="",legend.title=element_blank())+
  scale_fill_manual(values = c("Whole"="gray90", "White"="gray70", "Black"="gray50","Hispanic"="gray35"))
anesplot
ggsave("Goren_Figure1.pdf")
