rm(list=ls())
library(Rmisc)
library(reshape2)
library(reshape) # for function "melt" that transforms data from wide to long (as in Stata)
library(extrafont) # to customize plot fonts
# read csv data
d0<-read.csv("~/Dropbox/ida & jorge/Sep2016/data/csv/data oct 8.csv")
# Entonces agarras la columna que se llama “earn” y para cada “id”, para cada “section”, 
# calculas el average de “earn”. 
# El resultado es que cada individuo tiene 3 numberos (uno para cada section) 
# tons son 170x3 data points nuevos. 
# 
# El “type=1” dice que la section 2 si es section 2 en efecto y section 3 es section 3. 
# Si el type=0 entonces section 2 es en verdad section 3 y section 3 es en verdad section 2 
# y hay que renombrarlos
d<-subset(d0,select=c("id","section","earn","type"))
d$section.new<-d$section
d$section.new[d$type==0 & d$section==2]<-3
d$section.new[d$type==0 & d$section==3]<-2

d$section<-NULL
df<-as.data.frame(aggregate(earn~section.new+id,data=d, FUN="mean"))

filename="~/Dropbox/ida & jorge/Sep2016/data/csv/Av earn by id and section.csv"

write.csv(df,file=filename)

