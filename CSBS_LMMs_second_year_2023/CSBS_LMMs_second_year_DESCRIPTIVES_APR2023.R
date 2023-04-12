###############################################################################

# This code was utilized for the manuscript:
# Hooker, JL, Delehanty, A, Slate, E, & Wetherby, AM. Diagnostic Group  
# Differences in Social Communication Early and Late in the Second Year 
# cite paper here "in preparation (Manuscript in preparation).

# Code written by Jessica Hooker with input from Elizabeth Slate (March 2023)

# Brief description of the data:
## The data set comprised 385 children with two assessments on a social 
## communication measure (BS), diagnostic assessments measures (ADOS, MSEL), 
## demographic variables, and a grouping variable to indicate 
## diagnostic group (StudyDX_final: ASD, DD, TD)

# The primary goal of this study was to examine differences between diagnostic 
## groups in social communication skills from early to late in the second 
## year of life. We estimated linear mixed models for each subscale and 
## composite on the BS measure. 

# Brief Code Description:
## This file comprises the syntax for the data pre-processing, and 
## exploratory data analysis including visualization of key variables.
## This file also include the comparison of sample demographics and 
## characteristics by the grouping variable (StudyDX).

# This file does not include all analyses reported in the manuscript.

###############################################################################

# load libraries
library(readxl)
library(psych)
library(ggplot2)
library(gridExtra)
library(grid)
library(car)
library(dunn.test)
library(dplyr)

# data pre-processing --------------------------------------------------------

# import the raw data set
filename  =  " "
L2YBS = read_xlsx(filename,sheet  =  "early-late2nd_reduced"  )
View(L2YBS)
nrow(L2YBS)
# drop unnecessary columns
as.data.frame(colnames(L2YBS))
bsdata <-  L2YBS[, -c(1,3:16,19:32,38:79,150,151,231,235,236,
                      245,247:251,253:257,259:263,265:270,272:274,277:280)]
as.data.frame(colnames(bsdata))
# drop children under 12mos
bsdata <- bsdata[bsdata$BSCAge_1>= 12.00,]
nrow(bsdata)
# drop children that shouldn't be included
bsdata <- subset(bsdata, Include == 1)
nrow(bsdata)
str(bsdata, list.len = ncol(bsdata))
as.data.frame(colnames(bsdata))

# Preliminary Descriptives by Group --------------------------------
require(psych)
attach(bsdata)

# demographic data and group differences (StudyDX_final)
## check for missing data, replace with NR
colSums(is.na(bsdata[,c("Sex", "ChildRace", "Hispanic", "MomAge", "MomEd" )]))
bsdata$ChildRace[is.na(bsdata$ChildRace)] <- "NR"
## review group count
table (StudyDX_final)

## child sex
table(bsdata$Sex,bsdata$StudyDX_final)
round(prop.table(table(bsdata$Sex,bsdata$StudyDX_final),2),3)*100
chisq.test(bsdata$StudyDX_final,bsdata$Sex)

## child race
table(ChildRace,StudyDX_final)
round(prop.table(table(ChildRace,StudyDX_final),2),3)*100
chisq.test(bsdata$StudyDX_final,bsdata$ChildRace)

## child ethnicity
table(Hispanic,StudyDX_final)
round(prop.table(table(Hispanic,StudyDX_final),2),3)*100
chisq.test(bsdata$StudyDX_final,bsdata$Hispanic)

## maternal age and education descriptives
describeBy(bsdata[,c(154,156)], group  =  StudyDX_final)

### maternal age group differences
momageAOV  <-  aov( MomAge~StudyDX_final, data = bsdata)
summary(momageAOV)
coefficients(momageAOV)
#### check for homogeneity of variance
plot(momageAOV,1)
#### run LeveneTest--want non-significant
leveneTest(MomAge~StudyDX_final, data = bsdata, center = mean)
#### check for normality of residuals
plot(momageAOV,2)
#### run shapiro-wilk test--want non-significant
momageAOV_resids  <-  residuals(object  =  momageAOV)
shapiro.test(momageAOV_resids)
#### posthoc
TukeyHSD(momageAOV)

### maternal education group differences
momEdAOV  <-  aov( MomEd~StudyDX_final, data = bsdata)
summary(momEdAOV)
coefficients(momEdAOV)
#### check for homogeneity of variance
plot(momEdAOV,1)
#### run LeveneTest--want non-significant
leveneTest(MomEd~StudyDX_final, data = bsdata, center = mean)
#### check for normality of residuals
plot(momEdAOV,2)
#### run shapiro-wilk test--want non-significant
momEdAOV_resids  <-  residuals(object = momEdAOV)
shapiro.test(momEdAOV_resids)
#### non-parametric Kruskal-wallis due to
#### violation of normality of residuals & homogeneity of variance
kruskal.test(MomEd~StudyDX_final, data = bsdata)
dunn.test(bsdata$MomEd,bsdata$StudyDX_final, method = "bonferroni")

# diagnostic assessment data--age,of assessment, subscales, composites
describeBy(bsdata[,c(159,162,161,163,168,170,169,171:173)], 
           group  =  StudyDX_final)
## ADOS age group differences
ADOSageAOV <- aov(ADOS_age~StudyDX_final, data = bsdata)
summary(ADOSageAOV)
### check for homogeneity of variance
plot(ADOSageAOV,1)
### run LeveneTest--want non-significant
leveneTest(ADOS_age~StudyDX_final, data = bsdata, center = mean)
### check for normality of residuals
plot(ADOSageAOV,2)
### run shapiro-wilk test--want non-significant
ADOSageAOV_resids <- residuals(object = ADOSageAOV)
shapiro.test(ADOSageAOV_resids)
### non-parametric Kruskal-wallis due to
### violation of normality of residuals
kruskal.test(ADOS_age~StudyDX_final, data = bsdata)
dunn.test(bsdata$ADOS_age,bsdata$StudyDX_final, method = "bonferroni")

## ADOS SA CSS group differences
ADOS_SA_AOV <- aov(SACSS~StudyDX_final, data = bsdata)
summary(ADOS_SA_AOV)
### check for homogeneity of variance
plot(ADOS_SA_AOV,1)
### run LeveneTest--want non-significant
leveneTest(SACSS~StudyDX_final, data = bsdata, center = mean)
### check for normality of residuals
plot(ADOS_SA_AOV,2)
### run shapiro-wilk test--want non-significant
ADOS_SA_AOV_resids <- residuals(object = ADOS_SA_AOV)
shapiro.test(ADOS_SA_AOV_resids)
### non-parametric Kruskal-wallis due to
### violation of normality of residuals
kruskal.test(SACSS~StudyDX_final, data = bsdata)
dunn.test(bsdata$SACSS,bsdata$StudyDX_final, method = "bonferroni")

## ADOS RRB CSS group differences
ADOS_RRB_AOV <- aov(RRBCSS~StudyDX_final, data = bsdata)
summary(ADOS_RRB_AOV)
### check for homogeneity of variance
plot(ADOS_RRB_AOV,1)
### run LeveneTest--want non-significant
leveneTest(RRBCSS~StudyDX_final, data = bsdata, center = mean)
### check for normality of residuals
plot(ADOS_RRB_AOV,2)
### run shapiro-wilk test--want non-significant
ADOS_RRB_AOV_resids <- residuals(object = ADOS_RRB_AOV)
shapiro.test(ADOS_RRB_AOV_resids)
### non-parametric Kruskal-wallis due to
### violation of normality of residuals and homogeneity of variance
kruskal.test(RRBCSS~StudyDX_final, data = bsdata)
dunn.test(bsdata$RRBCSS,bsdata$StudyDX_final, method = "bonferroni")

## ADOS Total CSS group differences
ADOS_total_AOV <- aov(SARRBCSS~StudyDX_final, data = bsdata)
summary(ADOS_total_AOV)
### check for homogeneity of variance
plot(ADOS_total_AOV,1)
### run LeveneTest--want non-significant
leveneTest(SARRBCSS~StudyDX_final, data = bsdata, center = mean)
### check for normality of residuals
plot(ADOS_total_AOV,2)
### run shapiro-wilk test--want non-significant
ADOS_total_AOV_resids <- residuals(object = ADOS_total_AOV)
shapiro.test(ADOS_total_AOV_resids)
### non-parametric Kruskal-wallis due to
### violation of normality of residuals and homogeneity of variance
kruskal.test(SARRBCSS~StudyDX_final, data = bsdata)
dunn.test(bsdata$SARRBCSS,bsdata$StudyDX_final, method = "bonferroni")

# Mullen Age group differences
MSEL_age_AOV <- aov(MSEL_age~StudyDX_final, data = bsdata)
summary(MSEL_age_AOV)
## check for homogeneity of variance
plot(MSEL_age_AOV,1)
## run LeveneTest--want non-significant
leveneTest(MSEL_age~StudyDX_final, data = bsdata, center = mean)
## check for normality of residuals
plot(MSEL_age_AOV,2)
## run shapiro-wilk test--want non-significant
MSEL_age_AOV_resids <- residuals(object = MSEL_age_AOV)
shapiro.test(MSEL_age_AOV_resids)
## non-parametric Kruskal-wallis due:
## violation of normality of residuals and homogeneity of variance
kruskal.test(MSEL_age~StudyDX_final, data = bsdata)
dunn.test(bsdata$MSEL_age,bsdata$StudyDX_final, method = "bonferroni")

## Mullen FMT group differences
MSEL_FMT_AOV <- aov(FMT~StudyDX_final, data = bsdata)
summary(MSEL_FMT_AOV)
### check for homogeneity of variance
plot(MSEL_FMT_AOV,1)
### run LeveneTest--want non-significant
leveneTest(FMT~StudyDX_final, data = bsdata, center = mean)
### check for normality of residuals
plot(MSEL_FMT_AOV,2)
### run shapiro-wilk test--want non-significant
MSEL_FMT_AOV_resids <- residuals(object = MSEL_FMT_AOV)
shapiro.test(MSEL_FMT_AOV_resids)
### non-parametric Kruskal-wallis due to
### violation of normality of residuals and homogeneity of variance
kruskal.test(FMT~StudyDX_final, data = bsdata)
dunn.test(bsdata$FMT,bsdata$StudyDX_final, method = "bonferroni")

## Mullen VRT group differences
MSEL_VRT_AOV <- aov(VRT~StudyDX_final, data = bsdata)
summary(MSEL_VRT_AOV)
### check for homogeneity of variance
plot(MSEL_VRT_AOV,1)
### run LeveneTest--want non-significant
leveneTest(VRT~StudyDX_final, data = bsdata, center = mean)
### check for normality of residuals
plot(MSEL_VRT_AOV,2)
### run shapiro-wilk test--want non-significant
MSEL_VRT_AOV_resids <- residuals(object = MSEL_VRT_AOV)
shapiro.test(MSEL_VRT_AOV_resids)
### non-parametric Kruskal-wallis due to
### violation of normality of residuals and homogeneity of variance
kruskal.test(VRT~StudyDX_final, data = bsdata)
dunn.test(bsdata$VRT,bsdata$StudyDX_final, method = "bonferroni")

## Mullen RLT group differences
MSEL_RLT_AOV <- aov(RLT~StudyDX_final, data = bsdata)
summary(MSEL_RLT_AOV)
### check for homogeneity of variance
plot(MSEL_RLT_AOV,1)
### run LeveneTest--want non-significant
leveneTest(RLT~StudyDX_final, data = bsdata, center = mean)
### check for normality of residuals
plot(MSEL_RLT_AOV,2)
### run shapiro-wilk test--want non-significant
MSEL_RLT_AOV_resids <- residuals(object = MSEL_RLT_AOV)
shapiro.test(MSEL_RLT_AOV_resids)
### non-parametric Kruskal-wallis due to 
### violation of normality of residuals and homogeneity of variance
kruskal.test(RLT~StudyDX_final, data = bsdata)
dunn.test(bsdata$RLT,bsdata$StudyDX_final, method = "bonferroni")

## Mullen ELT group differences
MSEL_ELT_AOV <- aov(ELT~StudyDX_final, data = bsdata)
summary(MSEL_ELT_AOV)
### check for homogeneity of variance
plot(MSEL_ELT_AOV,1)
### run LeveneTest--want non-significant
leveneTest(ELT~StudyDX_final, data = bsdata, center = mean)
### check for normality of residuals
plot(MSEL_ELT_AOV,2)
### run shapiro-wilk test--want non-significant
MSEL_ELT_AOV_resids <- residuals(object = MSEL_ELT_AOV)
shapiro.test(MSEL_ELT_AOV_resids)
### non-parametric Kruskal-wallis due:
### violation of normality of residuals and homogeneity of variance
kruskal.test(ELT~StudyDX_final, data = bsdata)
dunn.test(bsdata$ELT,bsdata$StudyDX_final, method = "bonferroni")

## Mullen ELC group differences
MSEL_LCSS_AOV <- aov(LCSS~StudyDX_final, data = bsdata)
summary(MSEL_LCSS_AOV)
### check for homogeneity of variance
plot(MSEL_LCSS_AOV,1)
### run LeveneTest--want non-significant
leveneTest(LCSS~StudyDX_final, data = bsdata, center = mean)
### check for normality of residuals
plot(MSEL_LCSS_AOV,2)
### run shapiro-wilk test--want non-significant
MSEL_LCSS_AOV_resids <- residuals(object = MSEL_LCSS_AOV)
shapiro.test(MSEL_LCSS_AOV_resids)
### non-parametric Kruskal-wallis due:
### violation of normality of residuals and homogeneity of variance
kruskal.test(LCSS~StudyDX_final, data = bsdata)
dunn.test(bsdata$LCSS,bsdata$StudyDX_final, method = "bonferroni")


# summary statistics for BS data, age,of assessment, subscales, composites
describeBy(bsdata[,c(9,15,23,30,64,36,44,68,54,61,72,75,80,85,93,100,134,
                     106,114,138,124,131,142,145)], group  =  StudyDX_final)


# Age Analysis ------------------------------------------------------------
# Time 1 age
ggplot(bsdata,
       aes(x = BSCAge_1 ,color = StudyDX_final,fill = StudyDX_final))+
  geom_density(lwd = 1.2, linetype = 1, alpha = 0.2)+
  scale_color_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
  scale_fill_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
  theme_classic()+
  coord_cartesian(xlim  =  c(min(BSCAge_1),max(BSCAge_1)))+
  theme(legend.position = "bottom")+
  labs(x = 'Corrected Age (Time 1)', y = "Density",
       color = "Diagnosis", fill = "Diagnosis")

# link for scores at time 1 with scores at time 2
bscols <- list(list(15,85),list(23,93),list(30,100),list(36,106),list(44,114),
               list(54,124),list(61,131),list(64,134), list(68,138), list(72,142),
               list(75,145))

par(mfrow = c(1,2))
for (x in bscols){
  df1 <- data.frame(cbind(bsdata$BSCAge_1,bsdata[,unlist(x[1])]))
  df2 <- data.frame(cbind(bsdata$BSCAge_2,bsdata[,unlist(x[2])]))
  plot(df1, 
       main = sprintf('First BS Age by %s',colnames(df1[2])),
       xlab = "Age (months)",ylab =colnames(df1[2]), pch = 19,
       col.sub = 'blue')
  reg <- lm(df1[,2]~ df1[,1], data = df1)
  abline(reg, col = "darkred", lwd = 3)
  
  plot(df2, 
       main = sprintf('Second BS Age by %s',colnames(df2[2])),
       xlab = "Age (months)",ylab =colnames(df2[2]), pch = 19,
       col.sub = 'blue')
  reg <- lm(df2[,2]~ df2[,1], data = df2)
  abline(reg, col = "darkred", lwd = 3)
}

# Time 2 age
ggplot(bsdata,
       aes(x = BSCAge_2 ,color = StudyDX_final,fill = StudyDX_final))+
  geom_density(lwd = 1.2, linetype = 1, alpha = 0.2)+
  scale_color_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
  scale_fill_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
  theme_classic()+
  coord_cartesian(xlim  =  c(min(BSCAge_2),max(BSCAge_2)))+
  theme(legend.position = "bottom")+
  labs(x = 'Corrected Age (Time 2)', y = "Density",
       color = "Diagnosis", fill = "Diagnosis")

ggplot(bsdata,
       aes(x = (BSCAge_2- BSCAge_1) ,color = StudyDX_final,fill = StudyDX_final))+
  geom_density(lwd = 1.2, linetype = 1, alpha = 0.2)+
  scale_color_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
  scale_fill_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(x = 'Time 2-Time 1 Age Difference', y = "Density",
       color = "Diagnosis", fill = "Diagnosis")


# time 1 age difference by diagnostic group
time1ageAOV <- aov( BSCAge_1~StudyDX_final, data = bsdata)
summary(time1ageAOV)
coefficients(time1ageAOV)
# check for homogeneity of variance
plot(time1ageAOV,1)
# run LeveneTest--want non-significant
leveneTest(`BS CAge_1`~StudyDX_final, data = bsdata, center = mean)
# check for normality of residuals
plot(time1ageAOV,2)
# run shapiro-wilk test--want non-significant
time1ageAOV_resids <- residuals(object  =  time1ageAOV)
shapiro.test(time1ageAOV_resids)
cohen.d(BSCAge_1~StudyDX_final, data = bsdata)
# non-parametric Kruskal-wallis due to violation of normality of residuals
kruskal.test(`BS CAge_1`~StudyDX_final, data = bsdata)
dunn.test(bsdata$`BS CAge_1`,bsdata$StudyDX_final, method  =  "bonferroni")

# time 2 age difference by diagnostic group
time2ageAOV <- aov( `BS CAge_2`~StudyDX_final, data = bsdata)
summary(time2ageAOV)
coefficients(time2ageAOV)
# check for homogeneity of variance
plot(time2ageAOV,1)
# run LeveneTest--want non-significant
leveneTest(`BS CAge_2`~StudyDX_final, data = bsdata, center = mean)
# check for normality of residuals
plot(time2ageAOV,2)
# run shapiro-wilk test--want non-significant
time2ageAOV_resids <- residuals(object  =  time1ageAOV)
shapiro.test(time2ageAOV_resids)

#non-parametric Kruskal-wallis due to violation of normality of residuals
kruskal.test(`BS CAge_2`~StudyDX_final, data = bsdata)
dunn.test(bsdata$`BS CAge_2`,bsdata$StudyDX_final, method  =  "bonferroni")

# difference between time points by diagnostic group
bsdata$bsAgediff <- bsdata$BSCAge_2 - bsdata$BSCAge_1

ageDiffAOV <- aov( bsAgediff~StudyDX_final, data = bsdata)
summary(ageDiffAOV)
coefficients(ageDiffAOV)
#check for homogeneity of variance
plot(ageDiffAOV,1)
#run LeveneTest--want non-significant
leveneTest(bsAgediff~StudyDX_final, data = bsdata, center = mean)
#check for normality of residuals
plot(ageDiffAOV,2)
#run shapiro-wilk test--want non-significant
ageDiffAOV_resids <- residuals(object  =  time1ageAOV)
shapiro.test(ageDiffAOV_resids)
#non-parametric Kruskal-wallis due to violation of normality of residuals
kruskal.test(bsAgediff~StudyDX_final, data = bsdata)
dunn.test(bsdata$`BS CAge_1`,bsdata$StudyDX_final, method  =  "bonferroni")

attach(bsdata)
describeBy(bsdata$bsAgediff, group  =  bsdata$StudyDX_final)

# Plots of scores by diagnostic group Time 1 and Time 2 ------------------------------

#create a dataframe with diagnosis and time one variables
bstime1 <- bsdata[,c(6,14,20,28,35,41,49,59,66,69,73,77,80)] # col values are wrong
bstime1 <- as.data.frame(bstime1)

# plot a histogram and density plot of each cluster at time 1
for (x in 2:9){
  a <- ggplot(bstime1,
              aes(x = bstime1[,x],color = StudyDX_final,fill = StudyDX_final))+
    geom_histogram( bins  =  10,
                    position  =  "identity", alpha = 0.45)+
    scale_color_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    scale_fill_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    theme_classic()+
    coord_cartesian(xlim  =  c(min(bstime1[x]),max(bstime1[x])))+
    theme(legend.position = "none")+
    labs(x = sprintf('%s (Time 1)',colnames(bstime1[x])), y = "Frequency",
         color = "Diagnosis", fill = "Diagnosis")
  b <- ggplot(bstime1,
              aes(x = bstime1[,x],color = StudyDX_final, fill = StudyDX_final))+
    geom_density(lwd = 1.2, linetype = 1, alpha = 0.2)+
    scale_color_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    scale_fill_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    coord_cartesian(xlim  =  c(min(bstime1[x]),max(bstime1[x])))+
    theme_classic()+
    theme(legend.position = "none")+
    labs(x = sprintf('%s (Time 1)',colnames(bstime1[x])) , y = "Density")
  legend <- legendGrob(labels  =  c('ASD','DD','TD'),nrow = 1,ncol = 3,
                       hgap = unit(0.5,"lines"),pch = 22, 
                       gp = gpar(color = c("#6C3A5C","#F3A712","#519872"),
                                 fill  =  c("#6C3A5C","#F3A712","#519872")))
  grid.arrange(a,b,nrow = 1, top = sprintf("Time 1: %s\n",colnames(bstime1[x])),
               bottom  =  legend)
}

# for the composite scores at time 1
for (x in 10:13){
  a <- ggplot(bstime1,
              aes(x = bstime1[,x],color = StudyDX_final,fill = StudyDX_final))+
    geom_histogram( bins  =  20, 
                    position  =  "identity", alpha = 0.45)+
    scale_color_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    scale_fill_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    theme_classic()+
    coord_cartesian(xlim  =  c(min(bstime1[x]),max(bstime1[x])))+
    theme(legend.position = "none")+
    labs(x = sprintf('%s (Time 1)',colnames(bstime1[x])), y = "Frequency",
         color = "Diagnosis", fill = "Diagnosis")
  b <- ggplot(bstime1,
              aes(x = bstime1[,x],color = StudyDX_final, fill = StudyDX_final))+
    geom_density(lwd = 1.2, linetype = 1, alpha = 0.2)+
    scale_color_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    scale_fill_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    coord_cartesian(xlim  =  c(min(bstime1[x]),max(bstime1[x])))+
    theme_classic()+
    theme(legend.position = "none")+
    labs(x = sprintf('%s (Time 1)',colnames(bstime1[x])) , y = "Density")
  legend <- legendGrob(labels  =  c('ASD','DD','TD'),nrow = 1,ncol = 3,
                       hgap = unit(0.5,"lines"),pch = 22, 
                       gp = gpar(color = c("#6C3A5C","#F3A712","#519872"),
                                 fill  =  c("#6C3A5C","#F3A712","#519872")))
  grid.arrange(a,b,nrow = 1, top = sprintf("Time 1: %s\n",colnames(bstime1[x])),
               bottom  =  legend)
}

# plot a histogram and density plot of each cluster at time 2 by group
bstime2 <- bsdata[,c(6,84,90,98,105,111,119,129,136,139,143,147,150)]
bstime2 <- as.data.frame(bstime2)
for (x in 2:9){
  a <- ggplot(bstime2,
              aes(x = bstime2[,x],color = StudyDX_final,fill = StudyDX_final))+
    geom_histogram( bins  =  10, 
                    position  =  "identity", alpha = 0.45)+
    scale_color_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    scale_fill_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    theme_classic()+
    coord_cartesian(xlim  =  c(min(bstime2[x]),max(bstime2[x])))+
    theme(legend.position = "none")+
    labs(x = sprintf('%s (Time 2)',colnames(bstime2[x])), y = "Frequency",
         color = "Diagnosis", fill = "Diagnosis")
  b <- ggplot(bstime2,
              aes(x = bstime2[,x],color = StudyDX_final, fill = StudyDX_final))+
    geom_density(lwd = 1.2, linetype = 1, alpha = 0.2)+
    scale_color_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    scale_fill_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    coord_cartesian(xlim  =  c(min(bstime2[x]),max(bstime2[x])))+
    theme_classic()+
    theme(legend.position = "none")+
    labs(x = sprintf('%s (Time 2)',colnames(bstime2[x])) , y = "Density")
  legend <- legendGrob(labels  =  c('ASD','DD','TD'),nrow = 1,ncol = 3,
                       hgap = unit(0.5,"lines"),pch = 22, 
                       gp = gpar(color = c("#6C3A5C","#F3A712","#519872"),
                                 fill  =  c("#6C3A5C","#F3A712","#519872")))
  grid.arrange(a,b,nrow = 1, top = sprintf("Time 2: %s\n",colnames(bstime2[x])),
               bottom  =  legend)
}

for (x in 9:13){
  a <- ggplot(bstime2,
              aes(x = bstime2[,x],color = StudyDX_final,fill = StudyDX_final))+
    geom_histogram( bins  =  20, 
                    position  =  "identity", alpha = 0.45)+
    scale_color_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    scale_fill_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    theme_classic()+
    coord_cartesian(xlim  =  c(min(bstime2[x]),max(bstime2[x])))+
    theme(legend.position = "none")+
    labs(x = sprintf('%s (Time 2)',colnames(bstime2[x])), y = "Frequency",
         color = "Diagnosis", fill = "Diagnosis")
  b <- ggplot(bstime2,
              aes(x = bstime2[,x],color = StudyDX_final, fill = StudyDX_final))+
    geom_density(lwd = 1.2, linetype = 1, alpha = 0.2)+
    scale_color_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    scale_fill_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    coord_cartesian(xlim  =  c(min(bstime2[x]),max(bstime2[x])))+
    theme_classic()+
    theme(legend.position = "none")+
    labs(x = sprintf('%s (Time 2)',colnames(bstime2[x])) , y = "Density")
  legend <- legendGrob(labels  =  c('ASD','DD','TD'),nrow = 1,ncol = 3,
                       hgap = unit(0.5,"lines"),pch = 22, 
                       gp = gpar(color = c("#6C3A5C","#F3A712","#519872"),
                                 fill  =  c("#6C3A5C","#F3A712","#519872")))
  grid.arrange(a,b,nrow = 1, top = sprintf("Time 2: %s\n",colnames(bstime2[x])),
               bottom  =  legend)
}

# Change score plots ------------------------------------------------------

attach(bsdata)
# create change scores form time 1 to time 2
bsdata$EmotionGazeDiff <- EmotionGazeWRS_2 - EmotionGazeWRS_1
bsdata$CommunicationDiff <- CommunicationWRS_2 - CommunicationWRS_1
bsdata$GesturesDiff <- GesturesWRS_2 - GesturesWRS_1
bsdata$SoundsDiff <- SoundsWRS_2 - SoundsWRS_1
bsdata$WordsDiff <- WordsWRS_2 - WordsWRS_1
bsdata$UnderstDiff <- UnderstandingWRS_2 - UnderstandingWRS_1
bsdata$ObjectDiff <- ObjectsWRS_2 - ObjectsWRS_1
bsdata$SocialDiff <- SocialWRS_2 - SocialWRS_1
bsdata$SpeechDiff <- SpeechWRS_2 - SpeechWRS_1
bsdata$SymbolicDiff <- SymbolicWRS_2 - SymbolicWRS_1
bsdata$TotalDiff <- TotalWRS_2 - TotalWRS_1

# correlations between time 1 and change scores
cor(bsdata[,c(20,28,35,41,44,59,66,69,73,77,80,202:212)])
as.data.frame(colnames(bsdata))

# make data subset
bsdiffs <- bsdata[,c(6,202:212)]
bsdiffs <- as.data.frame(bsdiffs)

# plots of change scores by diagnostic group
for (x in 2:8){
  a <- ggplot(bsdiffs,
              aes(x = bsdiffs[,x],color = StudyDX_final,fill = StudyDX_final))+
    geom_histogram( bins  =  10, 
                    position  =  "identity", alpha = 0.45)+
    scale_color_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    scale_fill_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    theme_classic()+
    coord_cartesian(xlim  =  c(min(bsdiffs[x]),max(bsdiffs[x])))+
    theme(legend.position = "none")+
    labs(x = sprintf('%s (Change Score)',colnames(bsdiffs[x])), y = "Frequency",
         color = "Diagnosis", fill = "Diagnosis")
  b <- ggplot(bsdiffs,
              aes(x = bsdiffs[,x],color = StudyDX_final, fill = StudyDX_final))+
    geom_density(lwd = 1.2, linetype = 1, alpha = 0.2)+
    scale_color_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    scale_fill_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    coord_cartesian(xlim  =  c(min(bsdiffs[x]),max(bsdiffs[x])))+
    theme_classic()+
    theme(legend.position = "none")+
    labs(x = sprintf('%s (Change Score)',colnames(bsdiffs[x])) , y = "Density")
  legend <- legendGrob(labels  =  c('ASD','DD','TD'),nrow = 1,ncol = 3,
                       hgap = unit(0.5,"lines"),pch = 22, 
                       gp = gpar(color = c("#6C3A5C","#F3A712","#519872"),
                                 fill  =  c("#6C3A5C","#F3A712","#519872")))
  grid.arrange(a,b,nrow = 1, top = sprintf("Change Score: %s\n",colnames(bsdiffs[x])),
               bottom  =  legend)
}

for (x in 9:12){
  a <- ggplot(bsdiffs,
              aes(x = bsdiffs[,x],color = StudyDX_final,fill = StudyDX_final))+
    geom_histogram( bins  =  20, 
                    position  =  "identity", alpha = 0.45)+
    scale_color_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    scale_fill_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    theme_classic()+
    coord_cartesian(xlim  =  c(min(bsdiffs[x]),max(bsdiffs[x])))+
    theme(legend.position = "none")+
    labs(x = sprintf('%s (Change Score)',colnames(bsdiffs[x])), y = "Frequency",
         color = "Diagnosis", fill = "Diagnosis")
  b <- ggplot(bsdiffs,
              aes(x = bsdiffs[,x],color = StudyDX_final, fill = StudyDX_final))+
    geom_density(lwd = 1.2, linetype = 1, alpha = 0.2)+
    scale_color_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    scale_fill_manual(values  = c("#6C3A5C","#F3A712","#519872"))+
    coord_cartesian(xlim  =  c(min(bsdiffs[x]),max(bsdiffs[x])))+
    theme_classic()+
    theme(legend.position = "none")+
    labs(x = sprintf('%s (Change Score)',colnames(bsdiffs[x])) , y = "Density")
  legend <- legendGrob(labels  =  c('ASD','DD','TD'),nrow = 1,ncol = 3,
                       hgap = unit(0.5,"lines"),pch = 22, 
                       gp = gpar(color = c("#6C3A5C","#F3A712","#519872"),
                                 fill  =  c("#6C3A5C","#F3A712","#519872")))
  grid.arrange(a,b,nrow = 1, top = sprintf("Change Score: %s\n",colnames(bsdiffs[x])),
               bottom  =  legend)
}

describeBy(bsdiffs[,c(2:12)], group  =  bsdiffs$StudyDX_final)


