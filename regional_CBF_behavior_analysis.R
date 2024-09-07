rm(list=ls())
library(lavaan)
library(stats)
library(Formula)
library(mgcv)
library(voxel)
library(knitr)
library(visreg)
library(drc)
library(nlme)
library(R.matlab);

project_path = 'G:';
rCBF_mat_path = paste0(project_path,'/rCBF.mat');
rCBF = readMat(rCBF_mat_path);
age_mat_path = paste0(project_path,'/age.mat');
age = readMat(age_mat_path);
sex_mat_path = paste0(project_path,'/sex.mat');
sex = readMat(sex_mat_path);
motion_mat_path = paste0(project_path,'/MRD.mat');
MRD = readMat(motion_mat_path);
SES_mat_path = paste0(project_path,'/SES.mat');
SES = readMat(SES_mat_path);
behavior_mat_path = paste0(project_path,'/Bayley.mat');
behavior = readMat(behavior_mat_path);

total.var = nrow(rCBF)
Behavior_rCBF <- data.frame(behavior = as.numeric(behavior));
Behavior_rCBF$AgeMonths <- age;
Behavior_rCBF$Sex <- factor(sex);
Behavior_rCBF$MRD <- MRD;
Behavior_rCBF$SES <- SES;

rCBF_gam_behavior <- matrix(0, total.var, 3);

for (i in 1:total.var) {
  tmp_rCBF <- rCBF[i,]
  print(i)
  rCBF_Gam <- gam(tmp_rCBF ~ behavior + s(AgeMonths, k=3) + Sex + MRD + SES, method = "REML", data = Behavior_rCBF); # control SES
  rCBF_gam_behavior[i, 1] <- summary(rCBF_Gam)$p.table[2, 3];
  rCBF_gam_behavior[i, 2] <- summary(rCBF_Gam)$p.table[2, 4];
}
rCBF_gam_behavior[, 3] <- p.adjust(rCBF_gam_behavior[, 2], "fdr");

library(ggplot2)
dev.off()
Fig <- visreg(rCBF_Gam, "behavior", xlab = "", ylab = "", line.par = list(col = 'blue'), fill = list(fill = 'gray'), gg = TRUE)
Fig <- Fig + theme_classic() + theme(axis.text=element_text(size=30, color='black'),
                                     axis.ticks.length = unit(0.25, "cm"),
                                     axis.ticks = element_line(color = 'black',size = 1),
                                     axis.title =element_text(size=30, family="sans"),
                                     axis.line = element_line(colour = "black",size = 1))
Fig <- Fig + coord_cartesian(xlim=c(70,130),ylim=c(5,80))
Fig <- Fig + labs(x='Bayley Cognitive Score',y='Regional CBF (ml/100g/min)',size=30)
Fig <- Fig + geom_point(colour="#595959",size=3,shape=19,alpha=0.85)
