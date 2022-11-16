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

RaceGFK <- read_dta("C:/Users/Caity/Downloads/RaceGFK.dta")
gfksub<-subset(RaceGFK, select=c(cons1, cons2, cons3, cons4, race))

gfkwhole<-gfksub[(!is.na(gfksub$cons1)&!is.na(gfksub$cons2)
                           &!is.na(gfksub$cons3)&!is.na(gfksub$cons4)),]


########create correlation matrices
#whole population
wholecor<-cor(gfkwhole[,1:4])
lowerwhole<-wholecor[lower.tri(wholecor)]
wh<-round(mean(lowerwhole), digits=2)

#white sample
gfkwhite<-subset(gfkwhole, race==1)
whitecor<-cor(gfkwhite[,1:4])
lowerwhite<-whitecor[lower.tri(whitecor)]
wi<-round(mean(lowerwhite), digits=2)

#black sample
gfkblack<-subset(gfkwhole, race==2)
blackcor<-cor(gfkblack[,1:4])
lowerblack<-blackcor[lower.tri(blackcor)]
b<-round(mean(lowerblack), digits=2)

#hispanic sample
gfkhisp<-subset(gfkwhole,race==3)
hispcor<-cor(gfkhisp[,1:4])
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
anesplot<-anesplot + labs(title="Average Correlation for Conservation Values \nby Race (Figure 1)",subtitle="",y="Average Correlation",x="Race")+
  theme(legend.position="",legend.title=element_blank())+
  scale_fill_manual(values = c("Whole"="gray90", "White"="gray70", "Black"="gray50","Hispanic"="gray35"))
anesplot
ggsave("Goren_Figure3.pdf")
