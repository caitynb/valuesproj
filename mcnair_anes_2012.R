####set personal work environment
setwd("C:/Users/Caity/Downloads/McNair_Values")

####clean out R environment
rm(list=ls())

####import dataset and load libraries
library(haven)
library(ggplot2)
anes2012<-read_dta("anes_timeseries_2012.dta")


####create new variables from original ANES variables before data cleaning
#moral traditionalism items
anes2012$moral2<-anes2012$trad_adjust
anes2012$moral1<-anes2012$trad_lifestyle
anes2012$moral3<-anes2012$trad_tolerant
anes2012$moral4<-anes2012$trad_famval

###### race and weights
anes2012$race<-anes2012$dem_raceeth_x
# 1 white, 2 black, 3 asian, 4 native, 5 hispanic, 6 non-hispanic other, -9 missing
anes2012$weight<-anes2012$weight_full

table(anes2012$race)

#remove negative values and replace with NA
anes2012$moral1<-ifelse(anes2012$moral1<0,NA,anes2012$moral1)
anes2012$moral2<-ifelse(anes2012$moral2<0,NA,anes2012$moral2)
anes2012$moral3<-ifelse(anes2012$moral3<0,NA,anes2012$moral3)
anes2012$moral4<-ifelse(anes2012$moral4<0,NA,anes2012$moral4)

#race needs special coding, the goal is to have Hispanic (originally identified by value 5)
anes2012$race<-ifelse(anes2012$race==3,NA,anes2012$race)
anes2012$race<-ifelse(anes2012$race==5,3,anes2012$race)
anes2012$race<-ifelse(anes2012$race<0|anes2012$race>3,NA,anes2012$race)

#recode items to make them reverse coded, so a 5 indicates a stronger 
# level of traditionalism and a 1 is a weak level of traditionalism
anes2012$moral1<-recode(anes2012$moral1,"1=5;2=4;3=3;4=2;5=1")
anes2012$moral4<-recode(anes2012$moral4,"1=5;2=4;3=3;4=2;5=1")

#next step of cleaning that makes the correlations easy is to subset the factors
# of interest (the moral traditionalism items and race)
aneswhole<-subset(anes2012,select=c(moral1,moral2,moral3,moral4,race))
aneswhole<-aneswhole[(!is.na(aneswhole$moral1)&!is.na(aneswhole$moral2)
                      &!is.na(aneswhole$moral3)&!is.na(aneswhole$moral4)),]

########create correlation matrices
#whole population
wholecor<-cor(aneswhole[,1:4])
lowerwhole<-wholecor[lower.tri(wholecor)]
wh<-format(round(mean(lowerwhole), digits=2),nsmall=2)

#white sample
aneswhite<-subset(aneswhole, race==1)
whitecor<-cor(aneswhite[,1:4])
lowerwhite<-whitecor[lower.tri(whitecor)]
wi<-format(round(mean(lowerwhite), digits=2),nsmall=2)

#black sample
anesblack<-subset(aneswhole, race==2)
blackcor<-cor(anesblack[,1:4])
lowerblack<-blackcor[lower.tri(blackcor)]
b<-format(round(mean(lowerblack), digits=2),nsmall=2)

#hispanic sample
aneshisp<-subset(aneswhole,race==3)
hispcor<-cor(aneshisp[,1:4])
lowerhisp<-hispcor[lower.tri(hispcor)]
h<-format(round(mean(lowerhisp), digits=2),nsmall=2)

###################correlation figure
#create dataframe for figure
average<-c(wh, wi, b, h)
race<-c("Whole","White","Black","Latinx")
dat<-cbind.data.frame(average,race)

#relevel to ensure ordering on ggplot matches
dat$race<-factor(dat$race, levels=c("Whole","White","Black","Latinx"))
dat$avg<-as.numeric(dat$average)
#ggplot coding

anes12mt<-ggplot(dat, aes(x=race, y=avg,fill=race,))+
  geom_bar(stat="identity")+
  theme_classic()+
  geom_text(aes(label=average), vjust=-0.2,color="black", size=4)+
  scale_fill_manual(values = c("Whole"="gray70", "White"="gray60", "Black"="gray40","Latinx"="gray25"))+
  labs(y="Mean Correlation",x="",title="")+theme(legend.position="",legend.title=element_blank())
anes12mt
ggsave(file="fig_anes12MT.png", anes12mt, width = 4, height = 4)
