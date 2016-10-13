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
d<-read.csv("~/Dropbox/ida & jorge/Sep2016/data/csv/data sept 21.csv")
sd.y<-read.csv("~/Dropbox/ida & jorge/Sep2016/data/csv/data_y123.csv")
d.ind<-read.csv("~/Dropbox/ida & jorge/Sep2016/data/csv/data sept 26.csv")
# reshape data
d.reshape<-read.csv("~/Dropbox/ida & jorge/Sep2016/data/csv/data sept 28.csv")
d.reshape<-d.reshape[order(d.reshape$id, d.reshape$section),] # order data
d.r<-subset(d.reshape,select=c("id","section","estimate"))
d.r$counter<-with(d.r,ave(section,id,section,FUN = seq_along))
d.r$section.new<-d.r$section+(d.r$counter)/10
d.r$section<-NULL
d.r$counter<-NULL
d.r<-reshape(d.r, idvar="id", timevar="section.new", direction="wide")
write.csv(d.r,file ="~/Dropbox/ida & jorge/Sep2016/data/csv/data_wide.csv")


d.ind$se_aU_aB<-d.ind$se_aU-d.ind$se_aB
d.ind$se_aO_aB<-d.ind$se_aO-d.ind$se_aB
d.ind$se_aO_aU<-d.ind$se_aO-d.ind$se_aU

d.ind$aUaB<-d.ind$aU-d.ind$aB
d.ind$aOaB<-d.ind$aU-d.ind$aB

d.ind$group[d.ind$gender1male==1&d.ind$cpt1yes==1]<-"male & cpt"
d.ind$group[d.ind$gender1male==1&d.ind$cpt1yes==0]<-"male & nocpt"
d.ind$group[d.ind$gender1male==0&d.ind$cpt1yes==1]<-"female & cpt"
d.ind$group[d.ind$gender1male==0&d.ind$cpt1yes==0]<-"female & nocpt"

d.malecpt<-d.ind[d.ind$gender1male==1 & d.ind$cpt1yes==1,]
d.malenocpt<-d.ind[d.ind$gender1male==1 & d.ind$cpt1yes==0,]
d.femalecpt<-d.ind[d.ind$gender1male==0 & d.ind$cpt1yes==1,]
d.femalenocpt<-d.ind[d.ind$gender1male==0 & d.ind$cpt1yes==0,]

l1<-list(d.ind,d.malecpt,d.malenocpt,d.femalecpt,d.femalenocpt)
groups<-c("all","male & cpt","male & nocpt","female & cpt","female & nocpt")


for (i in 1:length(l1)){
  dat<-l1[[i]] # escoger data frame i de la lista l1
  ####### cdf #########  
  setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/cdf/") # set directory for saving plots
  # a
  setEPS()
  postscript(paste("Empirical CDF aB, aU, aO for ",groups[i],".eps",sep=""))
  plot(ecdf(dat$aB),main=paste("Empirical CDF for ",groups[i],sep=""),xlab="x=bias",ylab="CDF",verticals=TRUE, do.points=FALSE, col='red')+ 
    plot(ecdf(dat$aU),verticals=TRUE, do.points=FALSE, add=TRUE, col='green')+
    plot(ecdf(dat$aO),verticals=TRUE, do.points=FALSE, add=TRUE, col='blue')
  legend(25,.3,c("aB","aU","aO"),col=c('red','green','blue'),lty=c(1,1,1),lwd=c(1,1,1))
  dev.off()
  
  # sd(a)
  setEPS()
  postscript(paste("Empirical CDF se(aB), se(se_aU), (se_aO) for ",groups[i],".eps",sep=""))
  plot(ecdf(dat$se_aB),main=paste("Empirical CDF for ",groups[i],sep=""),xlab="x=bias",ylab="CDF",verticals=TRUE, do.points=FALSE, col='red')+ 
    plot(ecdf(dat$se_aU),verticals=TRUE, do.points=FALSE, add=TRUE, col='green')+
    plot(ecdf(dat$se_aO),verticals=TRUE, do.points=FALSE, add=TRUE, col='blue')
  legend(4,.3,c("se_aB","se_aU","se_aO"),col=c('red','green','blue'),lty=c(1,1,1),lwd=c(1,1,1))
  dev.off()
  
  # sd - sd
  setEPS()
  postscript(paste("Empirical CDF diff se(a) for ",groups[i],".eps",sep=""))
  plot(ecdf(dat$se_aU_aB),main=paste("Empirical CDF for ",groups[i],sep=""),xlab="x=bias",ylab="CDF",verticals=TRUE, do.points=FALSE, col='red')+ 
    plot(ecdf(dat$se_aO_aB),verticals=TRUE, do.points=FALSE, add=TRUE, col='green')+
    plot(ecdf(dat$se_aO_aU),verticals=TRUE, do.points=FALSE, add=TRUE, col='blue')
  legend(-3,.3,c("se(aU)-se(aB)","se(aO)-se(aB)","se(aO)-se(aU)"),col=c('red','green','blue'),lty=c(1,1,1),lwd=c(1,1,1))
  dev.off()
  
  ####### density & barplots of means #########
  setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/density/")
  
  # a
  d1<-subset(dat,select=c("aB","aU","aO"))
  long<-melt(d1)
  png(paste("Density aB, aU, aO for ",groups[i],".png",sep = ""))
  print(ggplot(long,aes(x=value, fill=variable)) + geom_density(alpha=0.25)+theme_bw()+
          xlab('a')+ggtitle(paste("Density aB, aU, aO for ",groups[i],sep = "")))
  dev.off()
  
  
  setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/barplots/")
  long.sum <- summarySE(long, measurevar="value", groupvars=c("variable"))
  
  setEPS()
  postscript(paste("Means aB, aU, aO for ",groups[i],".eps",sep=""))
  print(ggplot(long.sum, aes(x=variable, y=value, fill=variable)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=value-ci, ymax=value+ci),width=.2,# Width of the error bars
                  position=position_dodge(.9))+theme_bw()+xlab('mean with 95% confidence interval')+
    ggtitle(paste("Group: ",groups[i],sep = "")))
  dev.off()
  
  
  # se(a)
  setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/density/")
  
  d1<-subset(dat,select=c("se_aB","se_aU","se_aO"))
  long<-melt(d1)
  png(paste("Density se_aB, se_aU, se_aO for ",groups[i],".png",sep = ""))
  print(ggplot(long,aes(x=value, fill=variable)) + geom_density(alpha=0.25)+theme_bw()+
          xlab('a')+ggtitle(paste("Density se_aB, se_aU, se_aO for ",groups[i],sep = "")))
  dev.off()
  
  
  setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/barplots/")
  long.sum <- summarySE(long, measurevar="value", groupvars=c("variable"))
  
  setEPS()
  postscript(paste("Means se_aB, se_aU, se_aO for ",groups[i],".eps",sep=""))
  print(ggplot(long.sum, aes(x=variable, y=value, fill=variable)) + 
          geom_bar(position=position_dodge(), stat="identity") +
          geom_errorbar(aes(ymin=value-ci, ymax=value+ci),width=.2,# Width of the error bars
                        position=position_dodge(.9))+theme_bw()+xlab('mean with 95% confidence interval')+
          ggtitle(paste("Group: ",groups[i],sep = "")))
  dev.off()
  
  
  
  # diff se(a)
  setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/density/")
  
  d1<-subset(dat,select=c("se_aU_aB","se_aO_aB","se_aO_aU"))
  long<-melt(d1)
  long$variable<-factor(long$variable,levels = c( "se_aU_aB" ,"se_aO_aB" ,"se_aO_aU"),
                        labels=c("se(aU)-se(aB)","se(aO)-se(aB)","se(aO)-se(aU)"))
 
  png(paste("Density se_aU_aB, se_aO_aB, se_aO for ",groups[i],".png",sep = ""))
  print(ggplot(long,aes(x=value, fill=variable)) + geom_density(alpha=0.25)+theme_bw()+
          xlab('a')+ggtitle(paste("Density se_aU_aB, se_aO_aB, se_aO_aU for ",groups[i],sep = "")))
  dev.off()
  
  setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/barplots/")
  long.sum <- summarySE(long, measurevar="value", groupvars=c("variable"))
  
  setEPS()
  postscript(paste("Means se_aU_aB, se_aO_aB, se_aO for ",groups[i],".eps",sep=""))
  print(ggplot(long.sum, aes(x=variable, y=value, fill=variable)) + 
          geom_bar(position=position_dodge(), stat="identity") +
          geom_errorbar(aes(ymin=value-ci, ymax=value+ci),width=.2,# Width of the error bars
                        position=position_dodge(.9))+theme_bw()+xlab('mean with 95% confidence interval')+
          ggtitle(paste("Group: ",groups[i],sep = "")))
  dev.off()
  
  
  #### scatterplots ###########
  setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/scatter/")
  setEPS()
  postscript(paste("Scatter aUaB vs se_aU_aB for ", groups[i],".eps",sep=""))
  print(ggplot(dat, aes(aUaB,se_aU_aB))+geom_point(aes(colour = factor(group)))+theme_bw()+
    xlab('aU-aB')+ylab('se(aU)-se(aB)'))
  dev.off()
  
  setEPS()
  postscript(paste("Scatter aOaB vs se_aO_aB for ", groups[i],".eps",sep=""))
  print(ggplot(dat, aes(aOaB,se_aO_aB))+geom_point(aes(colour = factor(group)))+theme_bw()+
          xlab('aO-aB')+ylab('se(aO)-se(aB)'))
  dev.off()
  
  setEPS()
  postscript(paste("Scatter se_aO_aB vs se_aU_aB for ", groups[i],".eps",sep=""))
  print(ggplot(dat, aes(se_aO_aB,se_aU_aB))+geom_point(aes(colour = factor(group)))+theme_bw()+
          xlab('se(aO)-se(aB)')+ylab('se(aU)-se(aB)'))
  dev.off()
  
  setEPS()
  postscript(paste("Scatter se_aB vs se_aU for ", groups[i],".eps",sep=""))
  print(ggplot(dat, aes(se_aB,se_aU))+geom_point(aes(colour = factor(group)))+theme_bw()+
          xlab('se(aB)')+ylab('se(aU)'))
  dev.off()
  
  setEPS()
  postscript(paste("Scatter se_aB vs se_aO for ", groups[i],".eps",sep=""))
  print(ggplot(dat, aes(se_aB,se_aO))+geom_point(aes(colour = factor(group)))+theme_bw()+
          xlab('se(aB)')+ylab('se(aO)'))
  dev.off()
  
  setEPS()
  postscript(paste("Scatter se_aU vs se_aO for ", groups[i],".eps",sep=""))
  print(ggplot(dat, aes(se_aU,se_aO))+geom_point(aes(colour = factor(group)))+theme_bw()+
          xlab('se(aU)')+ylab('se(aO)'))
  dev.off()
 
  }

data.scatter<-subset(d.ind,select=c("aUaB","aOaB","se_aB","se_aU","se_aO","se_aU_aB","se_aO_aB","group"))
long<-melt(data.scatter)
long.sum <- summarySE(long, measurevar="value", groupvars=c("variable","group"))
test<-subset(long.sum,select=c("variable","group","value"))
test$variable<-as.character(test$variable)
l1<-split(test,test$variable)

for (i in 1:length(l1)) {
  d<-l1[[i]]
  names(d)[names(d)=="value"] <- d[1,1]
  d$variable<-NULL
  if (i==1){data<-d }
  else {
    d$group<-NULL
    data<-cbind(d,data)  }
}

dat<-as.data.frame(data)
setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/scatter/")
setEPS()
postscript(paste("Scatter mean aUaB vs se_aU_aB by group.eps"))
print(ggplot(dat, aes(aUaB,se_aU_aB))+geom_point(aes(colour = factor(group)),size=6)+theme_bw()+
        xlab('mean(aU-aB)')+ylab('mean(se(aU)-se(aB))'))
dev.off()

setEPS()
postscript(paste("Scatter mean aOaB vs se_aO_aB by group.eps"))
print(ggplot(dat, aes(aOaB,se_aO_aB))+geom_point(aes(colour = factor(group)),size=6)+theme_bw()+
        xlab('mean(aO-aB)')+ylab('mean(se(aO)-se(aB))'))
dev.off()

setEPS()
postscript(paste("Scatter mean se_aO_aB vs se_aU_aB by group.eps"))
print(ggplot(dat, aes(se_aO_aB,se_aU_aB))+geom_point(aes(colour = factor(group)),size=6)+theme_bw()+
        xlab('mean(se(aO)-se(aB))')+ylab('mean(se(aU)-se(aB))'))
dev.off()

setEPS()
postscript(paste("Scatter mean se_aB vs se_aU for by group.eps"))
print(ggplot(dat, aes(se_aB,se_aU))+geom_point(aes(colour = factor(group)),size=6)+theme_bw()+
        xlab('mean(se(aB))')+ylab('mean(se(aU))'))
dev.off()

setEPS()
postscript(paste("Scatter mean se_aB vs se_aO by group.eps"))
print(ggplot(dat, aes(se_aB,se_aO))+geom_point(aes(colour = factor(group)),size=6)+theme_bw()+
        xlab('mean(se(aB))')+ylab('mean(se(aO))'))
dev.off()

setEPS()
postscript(paste("Scatter mean se_aU vs se_aO by group.eps"))
print(ggplot(dat, aes(se_aU,se_aO))+geom_point(aes(colour = factor(group)),size=6)+theme_bw()+
        xlab('mean(se(aU))')+ylab('mean(se(aO))'))
dev.off()

# ggplot(dat, aes(se_aU,se_aO))+geom_point(aes(colour = saliva1))+theme_bw()+
#   xlab('se(aU)')+ylab('se(aO)')

