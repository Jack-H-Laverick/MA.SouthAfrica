
#### Set up ####

rm(list=ls())

library(tidyverse)
library(sf)

#### Build relationship between depth and disturbance from other models ####

DistDat <- read.csv("../Shared data/Prop disturbed.csv")

OutlierOmitted<-DistDat[-which.min(DistDat$dist),]

mod0<-lm(log(dist)~depth+sediment+shore,OutlierOmitted)

summary(mod0)

mod<-step(mod0)

mod<-lm(log(dist)~depth+shore,OutlierOmitted)

summary(mod)

inshore=DistDat[DistDat$shore=='in',]

b0<-summary(mod)$coef['(Intercept)','Estimate']
b1<-summary(mod)$coef['depth','Estimate']
b2<-summary(mod)$coef['shoreoff','Estimate']

x<-20:200
y1<-b0+b1*x
y2<-b0+b1*x+b2
plot(dist~depth,OutlierOmitted,pch=16,xlim=c(0,200),ylim=c(0,0.7),xlab='disturbance')
points(dist~depth,data=inshore,col='red',pch=16)
lines(x,exp(b0)*exp(b1*x),col='red')
lines(x,exp(b0+b2)*exp(b1*x))
legend('topright',c('inhsore','offshore'),col=c(2,1),pch=c(16,16),lty=c(1,1))

mod<-lm(log(dist)~depth+shore,OutlierOmitted)

Domains <- readRDS("./Objects/Domains.rds")

NewData <-data.frame(depth= abs(Domains$Elevation), shore=c("in","off")) 

exp(predict(mod,NewData))

#### Create object to save ####

user_pdist <- expand.grid(Month = month.name, 
                          Habitat = c("Silt", "Sand", "Gravel"), 
                          Shore = c("Inshore", "Offshore")) %>% 
  mutate(Disturbance = case_when(Shore =="Inshore" ~ exp(predict(mod,NewData))[1],
                                 Shore =="Offshore" ~ exp(predict(mod,NewData))[2]))

saveRDS(user_pdist, "./Objects/Habitat disturbance.rds")

