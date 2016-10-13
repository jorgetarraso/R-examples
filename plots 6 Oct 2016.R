rm(list=ls()) # clean all data
library(plyr)
library(ggplot2) # for plotting
library(VennDiagram)
library(gplots)
library(Rmisc)
library(latticeExtra)
library(reshape2)
library(reshape) # for function "melt" that transforms data from wide to long (as in Stata)
library(extrafont) # to customize plot fonts
library(scales)
# read csv data
d<-read.csv("~/Dropbox/ida & jorge/Sep2016/data/csv/data sept 26.csv")
# reshape data
# Cinco metricas relevantes:
#   1)	aB
# 2)	(aB – aU)
# 3)	(aB – aO)
# 4)	(se_aB – se_aU) (para los confidence intervals si es el standar error de la a el que queremos en vez 
#de el standard deviation que es el standard error multiplicado por sqrt(10), no?)
# 5)	(se_aB – se_aO) (para los confidence intervals si es el standar error de la a el que queremos en vez 
#de el standard deviation que es el standard error multiplicado por sqrt(10), no?)
# 
# Para las metricas relevantes, ver si es con 95% confidence menor que cero, cero o mayor que cero. 
#Crear tres dummies (una para cada metrica) que tomen el valor -1, 0, 1 si son menor a cero, cero o mayor a cero respectivamente. 

# create confidence intervals for 1-5
# use critical value fro t dist. with 9 degrees of freedom (=2.262)
d$ab_lb<-d$aB-2.262*d$se_aB
d$ab_ub<-d$aB+2.262*d$se_aB

d$gr1<-0
d$gr1[d$ab_ub<0]<- -1
d$gr1[d$ab_lb>0]<- 1

# suponiendo que aB, aU y aO son independientes
d$abau_lb<- (d$aB-d$aU)-2.262*sqrt(d$se_aB^2+d$se_aU^2)
d$abau_ub<- (d$aB-d$aU)+2.262*sqrt(d$se_aB^2+d$se_aU^2)

d$abao_lb<- (d$aB-d$aO)-2.262*sqrt(d$se_aB^2+d$se_aO^2)
d$abao_ub<- (d$aB-d$aO)+2.262*sqrt(d$se_aB^2+d$se_aO^2)

d$gr2<-0
d$gr2[d$abau_ub<0]<- -1
d$gr2[d$abau_lb>0]<- 1

d$gr3<-0
d$gr3[d$abao_ub<0]<- -1
d$gr3[d$abao_lb>0]<- 1

F025<-4.0260

d$se_abau_lb<-(d$se_aB/d$se_aU)/F025
d$se_abau_ub<-(d$se_aB/d$se_aU)*F025

d$se_abao_lb<-(d$se_aB/d$se_aO)/F025
d$se_abauo_ub<-(d$se_aB/d$se_aO)*F025

d$gr4<-0
d$gr4[d$se_abau_ub<1]<- -1
d$gr4[d$se_abau_lb>1]<- 1

d$gr5<-0
d$gr5[d$se_abao_ub<1]<- -1
d$gr5[d$se_abao_lb>1]<- 1

test<-subset(d,select=c("id","aB","aU","aO","se_aB","se_aU","se_aO","se_abau_lb","se_abau_ub"))
test<-subset(d,select=c("id","se_aB","se_aU","se_abau_lb","se_abau_ub","gr4"))
test$F<-F025
test$stat<-test$se_aB/test$se_aU

#### plots
d_1<-d[d$gr1== -1,]
d0<-d[d$gr1== 0,]
d1<-d[d$gr1== 1,]

l1<-list(d_1,d0,d1)

groupnames<-c("aB<0","ab=0","ab>0")
setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/3D_plots/") # set directory for saving plots

for (i in 1:length(l1)){
  dat<-l1[[i]]
  dat$no<-1
  df<-as.data.frame(aggregate(no ~ gr2+gr4,data=dat,FUN="sum"))
  df$gr2<-as.factor(df$gr2)
  df$gr4<-as.factor(df$gr4)
  
  filename<-paste(groupnames[i],"_se_aB_aU.eps",sep="")
  setEPS()
  postscript(filename)
  print(cloud(no~gr2+gr4, df,panel.3d.cloud=panel.3dbars, 
        xbase=.4, ybase=.4,
        scales = list(arrows = FALSE, just = "right"), xlab = "aB-aU", ylab = "se_aB/se_aU",
        col.facet = level.colors(df$no, at = do.breaks(range(df$no), 20),
                                 col.regions = cm.colors,
                                 colors = TRUE),main=groupnames[i],
        colorkey = list(col = cm.colors, at = do.breaks(range(df$no), 20)),
        screen = list(z = 40, x = -30)       ))
  dev.off()
  
  df<-as.data.frame(aggregate(no ~ gr3+gr5,data=dat,FUN="sum"))
  df$gr3<-as.factor(df$gr3)
  df$gr5<-as.factor(df$gr5)
  
  filename<-paste(groupnames[i],"_se_aB_aO.eps",sep="")
  setEPS()
  postscript(filename)
  print(cloud(no~gr3+gr5, df,panel.3d.cloud=panel.3dbars, 
              xbase=.4, ybase=.4,
              scales = list(arrows = FALSE, just = "right"), xlab = "aB-aO", ylab = "se_aB/se_aO",
              col.facet = level.colors(df$no, at = do.breaks(range(df$no), 20),
                                       col.regions = cm.colors,
                                       colors = TRUE),main=groupnames[i],
              colorkey = list(col = cm.colors, at = do.breaks(range(df$no), 20)),
              screen = list(z = 40, x = -30)       ))
  dev.off()
}


### 
d1<-d[d$gender1male==1 & d$cpt1yes==1,]
d2<-d[d$gender1male==1 & d$cpt1yes==0,]
d3<-d[d$gender1male==0 & d$cpt1yes==1,]
d4<-d[d$gender1male==0 & d$cpt1yes==0,]

groupnames<-c("all","malecpt","malenocpt","femalecpt","femalenocpt")
l1<-list(d,d1,d2,d3,d4)
for (i in 1:length(l1)){
  dat<-l1[[i]]
  dat$no<-1
  df<-as.data.frame(aggregate(no ~ gr1+gr2,data=dat,FUN="sum"))
  df$gr1<-as.factor(df$gr1)
  df$gr2<-as.factor(df$gr2)
  
  filename<-paste(groupnames[i],"_aB_aU.eps",sep="")
  setEPS()
  postscript(filename)
  print(cloud(no~gr1+gr2, df,panel.3d.cloud=panel.3dbars, 
              xbase=.4, ybase=.4,
              scales = list(arrows = FALSE, just = "right"), xlab = "aB", ylab = "aB-aU",
              col.facet = level.colors(df$no, at = do.breaks(range(df$no), 20),
                                       col.regions = cm.colors,
                                       colors = TRUE),main=groupnames[i],
              colorkey = list(col = cm.colors, at = do.breaks(range(df$no), 20)),
              screen = list(z = 40, x = -30)       ))
  dev.off()
  
  df<-as.data.frame(aggregate(no ~ gr1+gr3,data=dat,FUN="sum"))
  df$gr1<-as.factor(df$gr1)
  df$gr3<-as.factor(df$gr3)
  
  filename<-paste(groupnames[i],"_aB_aO.eps",sep="")
  setEPS()
  postscript(filename)
  print(cloud(no~gr1+gr3, df,panel.3d.cloud=panel.3dbars, 
              xbase=.4, ybase=.4,
              scales = list(arrows = FALSE, just = "right"), xlab = "aB", ylab = "aB-aO",
              col.facet = level.colors(df$no, at = do.breaks(range(df$no), 20),
                                       col.regions = cm.colors,
                                       colors = TRUE),main=groupnames[i],
              colorkey = list(col = cm.colors, at = do.breaks(range(df$no), 20)),
              screen = list(z = 40, x = -30)       ))
  dev.off()
}

####

d$d1<-"aB<=0"
d$d1[d$gr1==1]<-"aB>0"
d$d2<-"aB-aU<=0"
d$d2[d$gr2==1]<-"aB-aU>0"
d$d3<-"aB-aO>=0"
d$d3[d$gr3==-1]<-"aB-aO<0"

d$t[d$d1=="aB>0" & d$d2=="aB-aU>0" & d$d3=="aB-aO<0"]<-"aB>0, (aB – aU)>0, (aB – aO)<0"
d$t[d$d1=="aB<=0" & d$d2=="aB-aU>0" & d$d3=="aB-aO<0"]<-"aB<=0, (aB – aU)>0, (aB – aO)<0"
d$t[d$d1=="aB>0" & d$d2=="aB-aU<=0" & d$d3=="aB-aO<0"]<-"aB>0, (aB – aU)<=0, (aB – aO)<0"
d$t[d$d1=="aB<=0" & d$d2=="aB-aU<=0" & d$d3=="aB-aO<0"]<-"aB<=0, (aB – aU)<=0, (aB – aO)<0"
d$t[d$d1=="aB>0" & d$d2=="aB-aU>0" & d$d3=="aB-aO>=0"]<-"aB>0, (aB – aU)>0, (aB – aO)>=0"
d$t[d$d1=="aB<=0" & d$d2=="aB-aU>0" & d$d3=="aB-aO>=0"]<-"aB<=0, (aB – aU)>0, (aB – aO)>=0"
d$t[d$d1=="aB>0" & d$d2=="aB-aU<=0" & d$d3=="aB-aO>=0"]<-"aB>0, (aB – aU)<=0, (aB – aO)>=0"
d$t[d$d1=="aB<=0" & d$d2=="aB-aU<=0" & d$d3=="aB-aO>=0"]<-"aB<=0, (aB – aU)<=0, (aB – aO)>=0"

d$group[d$gender1male==1 & d$cpt1yes==1]<-"male & cpt"
d$group[d$gender1male==1 & d$cpt1yes==0]<-"male & nocpt"
d$group[d$gender1male==0 & d$cpt1yes==1]<-"female & cpt"
d$group[d$gender1male==0 & d$cpt1yes==0]<-"female & nocpt"
d$type<-d$t
d$t<-NULL
filename="~/Dropbox/ida & jorge/Sep2016/data/csv/data 170 ind with 8 types and 4 groups.csv"
write.csv(d,file=filename)

sel<-subset(d,select=c("type","group"))
sel$no<-1

df<-as.data.frame(aggregate(no ~ type+group,data=sel,FUN="sum"))
df$tot[df$group=="male & cpt"]<-length(which(d$group=="male & cpt"))
df$tot[df$group=="male & nocpt"]<-length(which(d$group=="male & nocpt"))
df$tot[df$group=="female & cpt"]<-length(which(d$group=="female & cpt"))
df$tot[df$group=="female & nocpt"]<-length(which(d$group=="female & nocpt"))

df$perc<-df$no/df$tot*100

filename="~/Dropbox/ida & jorge/Sep2016/data/csv/by 8 types and 4 groups.csv"
write.csv(df,file=filename)

setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/barplots/") # set directory for saving plots

png("types stacked by group.png",width=800,height=600)
print(ggplot(df,aes(x=group,y=no,fill=type))  +
        geom_bar(position = "fill",stat = "identity") + 
        scale_y_continuous(labels = percent_format())+theme_bw()+ylab("")+xlab(""))
dev.off()

png("groups stacked by type.png",width=800,height=1000)
ggplot(df,aes(x=type,y=no,fill=group))  +
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous(labels = percent_format())+theme_bw()+ylab("")+xlab("")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()
dev.off()


#############


t1<-table(d$gr1)
t2<-table(d$gr2)
t3<-table(d$gr3)
t4<-table(d$gr4)
t5<-table(d$gr5)

t6<-table(d$gr1,d$gr2)
t7<-table(d$gr1,d$gr3 )
t8<-table(d$gr1 ,d$gr4 )
t9<-table(d$gr1 ,d$gr5 )

t10<-table(d$gr2 ,d$gr3 )
t11<-table(d$gr2 ,d$gr4 )
t12<-table(d$gr3 ,d$gr5 )

list_all<-list(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12)


groupnames<-unique(d$group) # unique values in groupnames

# loop over groupnames
for (i in groupnames){
  t1<-table(d$gr1[d$group==i])
  t2<-table(d$gr2[d$group==i])
  t3<-table(d$gr3[d$group==i])
  t4<-table(d$gr4[d$group==i])
  t5<-table(d$gr5[d$group==i])
  
  t6<-table(d$gr1[d$group==i],d$gr2[d$group==i])
  t7<-table(d$gr1[d$group==i],d$gr3[d$group==i])
  t8<-table(d$gr1[d$group==i],d$gr4[d$group==i])
  t9<-table(d$gr1[d$group==i],d$gr5[d$group==i])
  
  t10<-table(d$gr2[d$group==i],d$gr3[d$group==i])
  t11<-table(d$gr2[d$group==i],d$gr4[d$group==i])
  t12<-table(d$gr3[d$group==i],d$gr5[d$group==i])
  
  l1<-list(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12)
  assign(paste("list",i,sep="_"),l1)
  
}
