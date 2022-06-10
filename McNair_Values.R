library(haven)
anes_timeseries_2016_sav <- read_sas("anes_timeseries_2016_sav.zip")
library(data.table)
egal<-data.table(anes2016$V162243, anes2016$V162244, anes2016$V162245, anes2016$V162246)
colnames(egal)<-c("egal_donecess","egal_worryless","egal_notbigprob","egal_fewerprobs")
trad<-data.table(anes2016$V162207, anes2016$V162208, anes2016$V162209, anes2016$V162210)
colnames(trad)<-c("trad_adjmoral","trad_lifestyl","trad_tolerant","trad_moretard")
#both of above cateogries are an agree to disagree scale
limtgov<-data.table(anes2016$V162183, anes2016$V162184, anes2016$V162185, anes2016$V162186)
colnames(limtgov)<-c("limtgov_govbig","limtgov_freemkt","limtgov_lessgovt","limtgov_regbus")
#this one they are given options
anescpv<-data.table(egal, trad, limtgov)
#merging the 3 cpv into one table

