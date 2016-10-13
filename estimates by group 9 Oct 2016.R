rm(list=ls())
library(plyr)
library(ggplot2) # for plotting
library(VennDiagram)
library(gplots)
library(Rmisc)
library(reshape2)
library(reshape) # for function "melt" that transforms data from wide to long (as in Stata)
library(extrafont) # to customize plot fonts
# read csv data
d<-read.csv("~/Dropbox/ida & jorge/Sep2016/data/csv/data_y123.csv")

d$z1<-d$y1-d$t
d$z2<-d$y2-d$t
d$z3<-d$y3-d$t

d$group[d$gender==1 & d$cpt==1]<-"male & cpt"
d$group[d$gender==1 & d$cpt==0]<-"male & nocpt"
d$group[d$gender==0 & d$cpt==1]<-"female & cpt"
d$group[d$gender==0 & d$cpt==0]<-"female & nocpt"

tot<-as.data.frame(table(d$group))
av<-as.data.frame(aggregate(d[, 10:12], list(d$group), mean,na.rm=TRUE))
var<-as.data.frame(aggregate(d[, 10:12], list(d$group), var,na.rm=TRUE))
av$se_aB<-sqrt(var$z1/tot$Freq)
av$se_aU<-sqrt(var$z2/tot$Freq)
av$se_aO<-sqrt(var$z3/tot$Freq)

av$aB<-av$z1
av$aU<-av$z2
av$aO<-av$z3
av$group<-av$Group.1

est<-subset(av,select = c("group","aB","aU","aO","se_aB","se_aU","se_aO"))

est$ab_lb<-est$aB-1.96*est$se_aB
est$ab_ub<-est$aB+1.96*est$se_aB

est$gr1<-0
est$gr1[est$ab_ub<0]<- -1
est$gr1[est$ab_lb>0]<- 1

# suponiendo que aB, aU y aO son independientes
est$abau_lb<- (est$aB-est$aU)-1.96*sqrt(est$se_aB^2+est$se_aU^2)
est$abau_ub<- (est$aB-est$aU)+1.96*sqrt(est$se_aB^2+est$se_aU^2)

est$abao_lb<- (est$aB-est$aO)-1.96*sqrt(est$se_aB^2+est$se_aO^2)
est$abao_ub<- (est$aB-est$aO)+1.96*sqrt(est$se_aB^2+est$se_aO^2)

est$gr2<-0
est$gr2[est$abau_ub<0]<- -1
est$gr2[est$abau_lb>0]<- 1

est$gr3<-0
est$gr3[est$abao_ub<0]<- -1
est$gr3[est$abao_lb>0]<- 1

F025<-1

est$se_abau_lb<-(est$se_aB/est$se_aU)/F025
est$se_abau_ub<-(est$se_aB/est$se_aU)*F025

est$se_abao_lb<-(est$se_aB/est$se_aO)/F025
est$se_abauo_ub<-(est$se_aB/est$se_aO)*F025

est$gr4<-0
est$gr4[est$se_abau_ub<1]<- -1
est$gr4[est$se_abau_lb>1]<- 1

est$gr5<-0
est$gr5[est$se_abao_ub<1]<- -1
est$gr5[est$se_abao_lb>1]<- 1

filename="~/Dropbox/ida & jorge/Sep2016/data/csv/group estimates.csv"
write.csv(est,file = filename)



dat<-subset(est,select = c("group","aB","aU","aO","se_aB","se_aU","se_aO"))
dat$gr<-c("v1","v2","v3","v4")
rownames(dat)<-dat$gr
dat$group<-NULL
dat$gr<-NULL
dat<-t(dat)
dat<-as.data.frame(dat)

d.new<-cbind(dat[1:3,],dat[4:6,])
colnames(d.new)<-c("v1","v2","v3","v4","s1","s2","s3","s4")
# v1 - female cpt
# v2 - female nocpt
# v3 - male cpt
# v4 - male nocpt

# A) male-cpt vs male-nocpt
# B) female-cpt vs female-nocpt
# C) male-cpt vs female-cpt
# D) male-nocpt vs female-nocpt

d.new$female_cpt_no<-(d.new$v1-d.new$v2)/sqrt(d.new$s1^2+d.new$s2^2)
d.new$male_cpt_no<-(d.new$v3-d.new$v4)/sqrt(d.new$s3^2+d.new$s4^2)
d.new$cpt_male_fem<-(d.new$v3-d.new$v1)/sqrt(d.new$s3^2+d.new$s1^2)
d.new$nocpt_male_fem<-(d.new$v4-d.new$v2)/sqrt(d.new$s4^2+d.new$s2^2)

d.new$female_cpt_no.lb<-(d.new$s1/d.new$s2)/1.2
d.new$female_cpt_no.ub<-(d.new$s1/d.new$s2)*1.2

d.new$male_cpt_no.lb<-(d.new$s3/d.new$s4)/1.2
d.new$male_cpt_no.ub<-(d.new$s3/d.new$s4)*1.2

d.new$cpt_male_fem.lb<-(d.new$s3/d.new$s1)/1.2
d.new$cpt_male_fem.ub<-(d.new$s3/d.new$s1)*1.2

d.new$nocpt_male_fem.lb<-(d.new$s4/d.new$s2)/1.2
d.new$nocpt_male_fem.ub<-(d.new$s4/d.new$s2)*1.2

filename="~/Dropbox/ida & jorge/Sep2016/data/csv/group_tests.csv"
write.csv(d.new,file = filename)





