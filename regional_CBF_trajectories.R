rm(list=ls())
library(segmented)
library(ggplot2)
library(extrafont)
library(drc)
library(nlme)
library(R.matlab);
library(mgcv);
library(visreg);

project_path = 'G:';
rCBF_mat_path = paste0(project_path,'/rCBF.mat');
rCBF = readMat(rCBF_mat_path);
age_mat_path = paste0(project_path,'/age.mat');
age = readMat(age_mat_path);
sex_mat_path = paste0(project_path,'/sex.mat');
sex = readMat(sex_mat_path);
sex = factor(sex)
motion_mat_path = paste0(project_path,'/MRD.mat');
MRD = readMat(motion_mat_path);

total.var = nrow(rCBF$rCBF);

rCBF_log_stats <- matrix(0, total.var, 8);
log_stats_mat <- file.path(project_path,'rCBF_log_stats.mat');
rCBF_log_MRD_sex_stats <- matrix(0, total.var, 10);
log_MRD_sex_stats_mat <- file.path(project_path,'rCBF_log_MRD_sex_stats.mat');

for (i in  1:total.var) {
  dat <- rCBF[i,]
  my.log <- lm(dat~log(age)+sex+MRD)
  rCBF_log_stats[i,1] <- summary(my.log)$coefficients[2,3]
  rCBF_log_stats[i,2] <- summary(my.log)$coefficients[2,4]
  rCBF_log_stats[i,4] <- summary(my.log)$adj.r.squared
  rCBF_log_stats[i,5] <- AIC(my.log)
  rCBF_log_stats[i,6] <- summary(my.log)$coefficients[2,1]
  rCBF_log_stats[i,7] <- summary(my.log)$coefficients[1,1]
  rCBF_log_stats[i,8] <- qnorm(summary(my.log)$coefficients[2,4] / 2, lower.tail=FALSE)
  rCBF_log_MRD_sex_stats[i,1] <- summary(my.log)$coefficients[4,1]
  rCBF_log_MRD_sex_stats[i,2] <- summary(my.log)$coefficients[4,3]
  rCBF_log_MRD_sex_stats[i,3] <- summary(my.log)$coefficients[4,4]
  rCBF_log_MRD_sex_stats[i,4] <- qnorm(summary(my.log)$coefficients[4,4] / 2, lower.tail=FALSE)
  rCBF_log_MRD_sex_stats[i,6] <- summary(my.log)$coefficients[3,1]
  rCBF_log_MRD_sex_stats[i,7] <- summary(my.log)$coefficients[3,3]
  rCBF_log_MRD_sex_stats[i,8] <- summary(my.log)$coefficients[3,4]
  rCBF_log_MRD_sex_stats[i,9] <- qnorm(summary(my.log)$coefficients[3,4] / 2, lower.tail=FALSE)

}
 rCBF_log_stats[, 3] <- p.adjust(rCBF_log_stats[, 2], "bonferroni");
 rCBF_log_MRD_sex_stats[, 5] <- p.adjust(rCBF_log_MRD_sex_stats[, 3], "bonferroni");
 rCBF_log_MRD_sex_stats[, 10] <- p.adjust(rCBF_log_MRD_sex_stats[, 8], "bonferroni");


i = c(41816)
dat <- rCBF[i,]
my.log.temp <- lm(dat~log(age)+sex+MRD)

Fig <- list()
Fig <- visreg(my.log.temp,"age",xlab = "Age (months)", ylabl="Regional CBF (ml/100g/min)",line.par=list(col='red'),fill=list(fill='light gray'),gg=TRUE)
Fig <- Fig + coord_cartesian(xlim=c(0,30),ylim=c(0,90))+
  labs(x='Age (months)',y='Regional CBF (ml/100g/min)',size=2)+
  theme(
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
