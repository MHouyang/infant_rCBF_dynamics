rm(list=ls())
library(segmented)
library(ggplot2)
library(extrafont)
library(drc)
library(nlme)
library(mgcv)
library(visreg)
temp <- read.csv("G:/globalCBF.csv", header=T)
cbf=temp$cbf
age=temp$age
gender=factor(temp$gender)
site=factor(temp$site)
my.lm <- lm(cbf~age, data=temp)
my.seg <- segmented(my.lm,
              seg.Z = ~ age,
              psi = 7)
my.log <- lm(cbf~log(age)+gender+site,data=temp)
summary(my.lm)
summary(my.seg)
summary(my.log)
Fig <- list()
Fig <- visreg(my.log,"age",xlab = "Age (months)", ylabl="Global CBF (ml/100g/min)",line.par=list(col='red'),fill=list(fill='light gray'),gg=TRUE)
Fig <- Fig + coord_cartesian(xlim=c(0,30),ylim=c(0,100))+
  labs(x='Age (months)',y='Global CBF (ml/100g/min)',size=2)+
  theme(
    text = element_text(family = "Arial"),
    axis.line = element_line(colour = "black",size = 1),
    axis.ticks = element_line(color = 'black',size = 1),
    axis.ticks.length.x = unit(0.25, "cm"),
    axis.ticks.length.y = unit(0.25, "cm"),
    axis.title.x=element_text(size=30, family="sans"),
    axis.text.x=element_text(color='black',size=30, family="sans"),
    axis.title.y=element_text(size=30, family="sans"),
    axis.text.y=element_text(color='black',size=30, family="sans"),
    plot.title=element_text(size=30),
    panel.border=element_blank(),
    panel.background=element_blank(),
    legend.position="none");
Fig <- Fig + scale_x_continuous(breaks = seq(0,30, by = 5))+  scale_y_continuous(breaks = seq(0,100, by = 20));
Fig <- Fig + geom_point(colour="#595959",size=4,shape=19,alpha=0.85)
