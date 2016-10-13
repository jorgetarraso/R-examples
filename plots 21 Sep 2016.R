rm(list=ls())
library(plyr)
library(ggplot2) # for plotting
library(VennDiagram)
library(gplots)
library(reshape) # for function "melt" that transforms data from wide to long (as in Stata)
library(extrafont) # to customize plot fonts
# read csv data
d<-read.csv("~/Dropbox/ida & jorge/Sep2016/data/csv/data sept 21.csv")
sd.y<-read.csv("~/Dropbox/ida & jorge/Sep2016/data/csv/data_y123.csv")

summary(d$deviation)

# deviation estaba en val abs y aqui le asigno el signo
d$deviation<-d$estimate-d$realtime
# set working directory

# generate subgroups (genera una condicion que le asigna el string si la condicion en brackets se cumple)
d$subgroup[d$gender==1&d$cpt==1]<-"male & cpt"
d$subgroup[d$gender==1&d$cpt==0]<-"male & nocpt"
d$subgroup[d$gender==0&d$cpt==1]<-"female & cpt"
d$subgroup[d$gender==0&d$cpt==0]<-"female & nocpt"

#d1 selecciona de d los renglones donde subgroup es male y cpt y todas las columnas de d
d1<-d[d$subgroup=="male & cpt",]
d2<-d[d$subgroup=="male & nocpt",]
d3<-d[d$subgroup=="female & cpt",]
d4<-d[d$subgroup=="female & nocpt",]



#NOW WE WILL DO CDF'S

# set working directory 
setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/cdf/")

# este primer renglon marca el inicio de donde se va a guardar el file y dev.off el final
setEPS()
postscript("empirical CDF by subgroup.eps",width=6,height=6)
plot(ecdf(d1$deviation),main="Empirical CDF",xlab="x=bias",ylab="CDF",verticals=TRUE, do.points=FALSE, col='red')+ 
  plot(ecdf(d2$deviation),verticals=TRUE, do.points=FALSE, add=TRUE, col='green')+
  plot(ecdf(d3$deviation),verticals=TRUE, do.points=FALSE, add=TRUE, col='black')+
  plot(ecdf(d4$deviation),verticals=TRUE, do.points=FALSE, add=TRUE, col='orange')
  legend(20,1,c("male & cpt","male & nocpt","female & cpt","female & nocpt"),col=c('red','green','black','orange'),lty=c(1,1,1,1),lwd=c(1,1,1,1))
# 20,0.9 are legend coordinates, next is legend labels and colors, finally, 
# lty - continuous lines, lwd - line width  
dev.off()


#NOW WE WILL DO density

setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/density/")

setEPS()
postscript("density by subgroup.eps",width=6,height=6)
print(ggplot(d, aes(x=deviation,color=subgroup))+geom_density()+theme_bw()+xlab('x=bias'))
dev.off()



m<-matrix(data=NA,nrow=1,ncol = 4)
m[1,1]<-mean(d1$deviation)
m[1,2]<-mean(d2$deviation)
m[1,3]<-mean(d3$deviation)
m[1,4]<-mean(d4$deviation)

std<-matrix(data=NA,nrow=1,ncol=4)
std[1,1]<-sqrt(var(d1$deviation))/sqrt(nrow(d1))
std[1,2]<-sqrt(var(d2$deviation))/sqrt(nrow(d1))
std[1,3]<-sqrt(var(d3$deviation))/sqrt(nrow(d1))
std[1,4]<-sqrt(var(d4$deviation))/sqrt(nrow(d1))

upper<-m+as.matrix(1.96*std)
lower<- m-as.matrix(1.96*std)

grid.newpage()
bp<- barplot2(m, beside = TRUE, horiz = TRUE, 
              plot.ci = TRUE, ci.u = upper, ci.l = lower,
              col = c("lightblue", "mistyrose", "lightcyan","green"), xlim = c(-20, 20),
              legend = c("male & cpt","male & nocpt","female & cpt","female & nocpt"))+
title(main = "Average bias and 95% confidence interval")

setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/barplots/")

setEPS()
postscript("barplot mean deviation.eps",width=7,height=7)
barplot2(m, beside = TRUE, horiz = TRUE, 
         plot.ci = TRUE, ci.u = upper, ci.l = lower,
         col = c("lightblue", "mistyrose", "lightcyan","green"), xlim = c(-20, 20),
         legend= c("male & cpt","male & nocpt","female & cpt","female & nocpt"))
dev.off()

#### plot densities and CDFs of y1, y2 and y3 for each interval
# 1. Densities

data.den<-subset(d.y,select=c("interval","y1","y2","y3"))  

setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/density/")

for (l in unique(d.y$interval)){
  d.sub<-data.den[data.den$interval==l,] # select data for interval
  d.sub$interval<-NULL # remove interval variable
  data<- melt(d.sub) # reshape from wide to long
  filename<-paste("int_",l,"_density.png",sep="") # define name for plot file
  title<-paste("Empirical density, Interval ",l,sep="") # plot title
  # setEPS()
  # postscript(filename,width=6,height=6)
  png(filename)
  print(ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.25)+theme_bw()+
          xlab('Estimate')+ggtitle(title))
  dev.off()
  
}# el print es el que me hace ver el plot y tambien que se guarde




  
# 2. CDFs
par(xpd=TRUE) # allow legend to go outside of plot region
setwd("~/Dropbox/ida & jorge/Sep2016/tex/graphs/cdf/")

for (l in unique(d.y$interval)){
  d.sub<-d.y[d.y$interval==l,] # choose rows of data corresponding to the interval
  y1<-d.sub$y1 # choose columns that correspond to estimates in Sec 1-3
  y2<-d.sub$y2
  y3<-d.sub$y3
  filename<-paste("int_",l,"_CDFs.eps",sep="") # define name for plot file
  title<-paste("Empirical CDF, Interval ",l,sep="") # plot title
  setEPS()
  postscript(filename,width=6,height=6)
  plot(ecdf(y1),main = title,xlab = "estimate",ylab = "CDF",verticals=TRUE, do.points=FALSE, col='red')+ 
    plot(ecdf(y2),verticals=TRUE, do.points=FALSE, add=TRUE, col='green')+  
    plot(ecdf(y3),verticals=TRUE, do.points=FALSE, add=TRUE, col='blue')
    legend(60,.4,c("Section 1", "Section 2","Section 3"),col=c('red','green','blue'),lty=c(1,1,1))
  dev.off()
}


# venn diagram
grid.newpage()
draw.pairwise.venn(22, 20, 11, category = c("correct shading", "a1>1"), 
                   lty = rep("blank",2), 
                   fill = c("light blue", "pink"), alpha = rep(0.5, 2), 
                   cat.pos = c(0, 0), cat.dist = rep(0.025, 2))

