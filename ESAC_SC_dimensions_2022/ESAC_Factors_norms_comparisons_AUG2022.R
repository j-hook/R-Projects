###############################################################################
# This code was utilized for the manuscript:
## Hooker, JL, Delehanty, A, Slate, E, & Wetherby, AM,
## Validating the Early Screening for Autism and Communication (ESAC) in 
## toddlers with and without social communication delay.

# Code written by Jessica Hooker with input from Elizabeth Slate (August 2022)

# Brief description of the data:
## The data set comprised 908 children who completed the Early Screening
## for Autism and Communication (ESAC) between the ages of 11.50 and 25.50 months 
## and a social communication assessment (BS) within two months. A sub-sample of 
## 477 children also completed a diagnostic assessment and received a diagnosis 
## of ASD, DD, or TD. 

# There were three aims of this analysis:
## 1. To examine the dimension structure of the social communication related items
## on the ESAC. 
## 2. To examine the relationship between the identified dimensions and age 
## in order to establish norm-referenced scoring for each dimension.
## 3. To explore differences on each dimension between children who are 
## typically developing and those with social communication delays as well as 
## between diagnostic groups in the sub-sample of children with a diagnostic
## assessment. 

# Brief Code Description:
## This file comprises the syntax for third aim of this project. Specifically, 
## this syntax include the upload of the new normative data for the two 
## identified dimensions of the ESAC: Social Factor ("socintFactor_std")
## and Symbolic Factor ("symFactor_std"). Normed scores for each factor were 
## first compared to children with social communication risk ("BS risk") and 
## those who are typically developing (BS-TD). Children with ASD or DD were 
## excluded from this analysis. Scores on the two factors were then compared 
## across diagnostic groups (ASD, DD, TD).

# This file does not include all analyses reported in the manuscript.

###############################################################################
library(readxl)
library(psych)
library(ggplot2)
library(car)
library(dplyr)

# APPLY NORMS SCORES BASED ON BS-TD to full sample 
## use dataframe with items already recoded and the factor scores created:
## esac2BSscrecode

# upload norms tables for merging

social_file <- ""
symbolic_file <- ""

socFactornorms <- read_excel(social_file)
str(socFactornorms)
symFactornorms <- read_excel(symbolic_file)
str(symFactornorms)

# merge the social factor norms
esac2BSscFactors <- merge(esac2BSscrecode,socFactornorms, 
                        by.x=c("age_groups","socintfactor"), 
                        by.y=c("age_groups","socintfactor_raw"), all.x = T)
View(esac2BSscFactors)
# merge the symbolic factor norms
esac2BSscFactors <- merge(esac2BSscFactors,symFactornorms, 
                        by.x=c("age_groups","symbfactor"), 
                        by.y = c("age_groups","symFactor_raw"),all.x = T)
str(esac2BSscFactors)
# create variables for BS-TD vs BS risk...excludes ASD and DD
bstd_vs_bsrisk <- ifelse(esac2BSscFactors$StudyDX=="TD" & 
                       esac2BSscFactors$BS_Risk==0 
                     |esac2BSscFactors$StudyDX=="NDO" & 
                       esac2BSscFactors$BS_Risk==0,"BSTD",
                     ifelse(esac2BSscFactors$StudyDX=="TD" & 
                              esac2BSscFactors$BS_Risk==1 
                            |esac2BSscFactors$StudyDX=="NDO" & 
                              esac2BSscFactors$BS_Risk==1,
                            "nonASDDD_bsrisk",NA
                            )
                     )
bstd_vs_bsrisk <- as.factor(bstd_vs_bsrisk)
esac2BSscFactors$bstd_vs_bsrisk <- bstd_vs_bsrisk

table(esac2BSscFactors$bstd_vs_bsrisk)
esac2BSscFactors$age_groups <- as.factor(esac2BSscFactors$age_groups)

################################################################################
# compare the BS-TD groups vs BSrisk

# social factor
ggplot(data=subset(esac2BSscFactors,!is.na(bstd_vs_bsrisk)),
       aes(x=socintFactor_std,color=bstd_vs_bsrisk, fill=bstd_vs_bsrisk))+
  geom_density(lwd=1.2, linetype=1, alpha=0.2)+
  scale_color_manual(values =c("#7b3294","#008837"))+
  scale_fill_manual(values =c("#7b3294","#008837"))+
  theme_bw()+
  theme(legend.position="right")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title="Social Interaction FactorDensity Curve by 
       BS Risk (ASD & DD excluded)\n",
       x="Social Interaction Factor Standard Score", y="Density",
       color="BS Risk", fill="BS Risk")

# symbolic factor
ggplot(data=subset(esac2BSscFactors,!is.na(bstd_vs_bsrisk)),
       aes(x=symFactor_std,color=bstd_vs_bsrisk, fill=bstd_vs_bsrisk))+
  geom_density(lwd=1.2, linetype=1, alpha=0.2)+
  scale_color_manual(values =c("#7b3294","#008837"))+
  scale_fill_manual(values =c("#7b3294","#008837"))+
  theme_bw()+
  theme(legend.position="right")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title="Symbolic Factor Density Curve by BS Risk (ASD & DD excluded)\n",
       x="Symbolic Factor Standard Score", y="Density",
       color="BS Risk", fill="BS Risk")

describeBy(socintFactor_std+symFactor_std~bstd_vs_bsrisk+age_groups, 
           data = esac2BSscFactors, mat=F)
describeBy(socintFactor_std+symFactor_std~bstd_vs_bsrisk, 
           data = esac2BSscFactors, mat=F)

require(car)
# test of BS-TD vs BS risk group (excludes ASD vs DD)
# social interaction Factor Score
BSrisk_socint_aov <- aov(socintFactor_std~bstd_vs_bsrisk, 
                       data=subset(esac2BSscFactors,!is.na(bstd_vs_bsrisk)))
summary(BSrisk_socint_aov)
coefficients(BSrisk_socint_aov)
## check for homogeneity of variance
plot(BSrisk_socint_aov,1)
## run LeveneTest--want non-significant
leveneTest(socintFactor_std~bstd_vs_bsrisk, data=esac2BSscFactors, center=mean)
## check for normality of residuals
plot(BSrisk_socint_aov,2)
## run shapiro-wilk test--want non-significant
BSrisk_socint_aov_resids <- residuals(object = BSrisk_socint_aov)
shapiro.test(BSrisk_socint_aov_resids)
## non-parametric test
kruskal.test(socintFactor_std~bstd_vs_bsrisk, 
             data=subset(esac2BSscFactors,!is.na(bstd_vs_bsrisk)))
## eta-squared effect size
kruskal_effsize(socintFactor_std~bstd_vs_bsrisk, 
                data=subset(esac2BSscFactors,!is.na(bstd_vs_bsrisk)))
## epsilon effect size
h4 <- unname(kruskal.test(socintFactor_std~bstd_vs_bsrisk, 
                        ata=subset(esac2BSscFactors,
                                   !is.na(bstd_vs_bsrisk)))$statistic)
h4
n4 <- sum(table(esac2BSscFactors$socintFactor_std,
              esac2BSscFactors$bstd_vs_bsrisk, useNA = "no"))
n4
epsilon4 <- h4*(n4+1)/(n4^2-1)
epsilon4

# Symbolic Factor Score
BSrisk_symbolic_aov <- aov(symFactor_std~bstd_vs_bsrisk, 
                         data=subset(esac2BSscFactors,!is.na(bstd_vs_bsrisk)))
summary(BSrisk_symbolic_aov)
coefficients(BSrisk_symbolic_aov)
## check for homogeneity of variance
plot(BSrisk_symbolic_aov,1)
## run LeveneTest--want non-significant
leveneTest(symFactor_std~bstd_vs_bsrisk, data=esac2BSscFactors, center=mean)
## check for normality of residuals
plot(BSrisk_symbolic_aov,2)
## run shapiro-wilk test--want non-significant
BSrisk_symbolic_aov_resids <- residuals(object = BSrisk_symbolic_aov)
shapiro.test(BSrisk_symbolic_aov_resids)
## non-parametric test
kruskal.test(symFactor_std~bstd_vs_bsrisk, 
             data=subset(esac2BSscFactors,!is.na(bstd_vs_bsrisk)))
## eta-squared effect size
kruskal_effsize(symFactor_std~bstd_vs_bsrisk, 
                data=subset(esac2BSscFactors,!is.na(bstd_vs_bsrisk)))
## epsilon-squared
h3 <- unname(kruskal.test(symFactor_std~bstd_vs_bsrisk, 
                        data=subset(esac2BSscFactors,
                                    !is.na(bstd_vs_bsrisk)))$statistic)
h3
n3 <- sum(table(esac2BSscFactors$symFactor_std,
              esac2BSscFactors$bstd_vs_bsrisk, useNA = "no"))
n3
epsilon3 <- h3*(n3+1)/(n3^2-1)
epsilon3

# Descriptives of samples by percentile cuts
# social interaction factor
# 2nd percentile
round(prop.table(table(esac2BSscFactors$bstd_vs_bsrisk, 
                       esac2BSscFactors$socintFactor_perc<=2, 
                       exclude = NA, 
                       dnn = c("BS risk", "At or Below 2nd Percentile")),
                 margin = 1),2)

round(prop.table(table(esac2BSscFactors$bstd_vs_bsrisk, 
                       esac2BSscFactors$socintFactor_perc<=5, 
                       exclude = NA, 
                       dnn = c("BS risk", "At or Below 5th Percentile")),
                 margin = 1),2)

round(prop.table(table(esac2BSscFactors$bstd_vs_bsrisk, 
                       esac2BSscFactors$socintFactor_perc<=9, 
                       exclude = NA, 
                       dnn = c("BS risk", "At or Below 9th Percentile")),
                 margin = 1),2)

# symbolic
round(prop.table(table(esac2BSscFactors$bstd_vs_bsrisk, 
                       esac2BSscFactors$symFactor_perc <=2, 
                       exclude = NA, 
                       dnn = c("BS risk", "At or Below 2nd Percentile")),
                 margin = 1),2)

round(prop.table(table(esac2BSscFactors$bstd_vs_bsrisk,
                       esac2BSscFactors$symFactor_perc<=5, 
                       exclude = NA, 
                       dnn = c("BS risk", "At or Below 5th Percentile")),
                 margin = 1),2)

round(prop.table(table(esac2BSscFactors$bstd_vs_bsrisk, 
                       esac2BSscFactors$symFactor_perc<=9, 
                       exclude = NA, 
                       dnn = c("BS risk", "At or Below 9th Percentile")),
                 margin = 1),2)


################################################################################

# compare the TD, ASD, and DD groups

# social factor
ggplot(data=subset(esac2BSscFactors,StudyDX != "NDO"),
       aes(x=socintFactor_std,color=StudyDX, fill=StudyDX))+
  geom_density(lwd=1.2, linetype=1, alpha=0.4)+
  scale_color_manual(values =c("#66c2a5","#fc8d62","#8da0cb"))+
  scale_fill_manual(values =c("#66c2a5","#fc8d62","#8da0cb"))+
  theme_bw()+
  theme(legend.position="right")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title="Social Interaction Factor Density Curve by Diagnostic Group\n",
       x="Social Interaction Factor Standard Score", y="Density",
       color="Diagnosis", fill="Diagnosis")

# symbolic factor
ggplot(data=subset(esac2BSscFactors, StudyDX != "NDO"),
       aes(x=symFactor_std,color=StudyDX, fill=StudyDX))+
  geom_density(lwd=1.2, linetype=1, alpha=0.4)+
  scale_color_manual(values =c("#66c2a5","#fc8d62","#8da0cb"))+
  scale_fill_manual(values =c("#66c2a5","#fc8d62","#8da0cb"))+
  theme_bw()+
  theme(legend.position="right")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title="Symbolic Factor Density Curve by Diagnostic Group\n",
       x="Symbolic Factor Standard Score", y="Density",
       color="Diagnosis", fill="Diagnosis")

require(psych)
describeBy(socintFactor_std+symFactor_std~StudyDX+age_groups, 
           data = subset(esac2BSscFactors,StudyDX != "NDO"), mat=F)
describeBy(socintFactor_std+symFactor_std~StudyDX, 
           data = subset(esac2BSscFactors,StudyDX != "NDO"), mat=F)


require(car)
# test of BS-TD vs BS risk group (excludes ASD vs DD)
# social interaction Factor Score
DX_socint_aov <- aov(socintFactor_std~StudyDX, 
                   data=subset(esac2BSscFactors,StudyDX != "NDO"))
summary(DX_socint_aov)
coefficients(DX_socint_aov)
## check for homogeneity of variance
plot(DX_socint_aov,1)
## run LeveneTest--want non-significant
leveneTest(socintFactor_std~StudyDX, 
           data=subset(esac2BSscFactors,StudyDX != "NDO"), center=mean)
## check for normality of residuals
plot(DX_socint_aov,2)
## run shapiro-wilk test--want non-significant
DX_socint_aov_resids <- residuals(object = DX_socint_aov)
shapiro.test(DX_socint_aov_resids)
## follow-up analyses if not violated
TukeyHSD(DX_socint_aov)
## no NDO data
esac2BSscFactorsexNDO <- subset(esac2BSscFactors,StudyDX != "NDO")
esac2BSscFactorsexNDO$StudyDX <- factor(esac2BSscFactorsexNDO$StudyDX, 
                                      exclude = "NDO")
pairwise.t.test(esac2BSscFactorsexNDO$socintFactor_std,
                esac2BSscFactorsexNDO$StudyDX,
                p.adjust.method = "holm")
## non-parametric test
kruskal.test(socintFactor_std~StudyDX, 
             data=subset(esac2BSscFactors,StudyDX != "NDO"))
dunn.test(esac2BSscFactorsexNDO$socintFactor_std,
          esac2BSscFactorsexNDO$StudyDX, method = "bonferroni")
kruskal_effsize(socintFactor_std~StudyDX, 
                data=subset(esac2BSscFactors,StudyDX != "NDO"))

h5 <- unname(kruskal.test(socintFactor_std~StudyDX, 
                        data=subset(esac2BSscFactors,
                                    StudyDX != "NDO"))$statistic)
h5
n5 <- nrow(esac2BSscFactorsexNDO)
n5
epsilon5 <- h5*(n5+1)/(n5^2-1)
epsilon5


# Symbolic Factor Score
DX_symbolic_aov <- aov(symFactor_std~StudyDX, 
                     data=subset(esac2BSscFactors,StudyDX != "NDO"))
summary(DX_symbolic_aov)
coefficients(DX_symbolic_aov)
## check for homogeneity of variance
plot(DX_symbolic_aov,1)
## run LeveneTest--want non-significant
leveneTest(symFactor_std~StudyDX, 
           data=subset(esac2BSscFactors,StudyDX != "NDO"), 
           center=mean)
## check for normality of residuals
plot(DX_symbolic_aov,2)
## run shapiro-wilk test--want non-significant
DX_symbolic_aov_resids <- residuals(object = DX_symbolic_aov)
shapiro.test(DX_symbolic_aov_resids)
## follow-up analyses if not violated
TukeyHSD(DX_symbolic_aov)
## no NDO data
pairwise.t.test(esac2BSscFactorsexNDO$symFactor_std,
                esac2BSscFactorsexNDO$StudyDX,
                p.adjust.method = "holm")
## non-parametric test
kruskal.test(symFactor_std~StudyDX, 
             data=subset(esac2BSscFactors,StudyDX != "NDO"))
dunn.test(esac2BSscFactorsexNDO$symFactor_std,
          esac2BSscFactorsexNDO$StudyDX, method = "bonferroni")
kruskal_effsize(symFactor_std~StudyDX, 
                data=subset(esac2BSscFactors,StudyDX != "NDO"))

h6 <- unname(kruskal.test(symFactor_std~StudyDX, 
                        data=subset(esac2BSscFactors,
                                    StudyDX != "NDO"))$statistic)
h6
n6 <- nrow(esac2BSscFactorsexNDO)
n6
epsilon6 <- h6*(n6+1)/(n6^2-1)
epsilon6


# Descriptives of samples by percentile cuts
# social interaction factor
# 2nd percentile
round(prop.table(table(esac2BSscFactors$StudyDX, 
                       esac2BSscFactors$socintFactor_perc<=2, 
                       exclude = NA, 
                       dnn = c("Diagnosis", "At or Below 2nd Percentile")),
                 margin = 1),2)

round(prop.table(table(esac2BSscFactors$StudyDX, 
                       esac2BSscFactors$socintFactor_perc<=5, 
                       exclude = NA, 
                       dnn = c("Diagnosis", "At or Below 5th Percentile")),
                 margin = 1),2)

round(prop.table(table(esac2BSscFactors$StudyDX, 
                       esac2BSscFactors$socintFactor_perc<=9, 
                       exclude = NA, 
                       dnn = c("Diagnosis", "At or Below 9th Percentile")),
                 margin = 1),2)

# symbolic factor 
round(prop.table(table(esac2BSscFactors$StudyDX,
                       esac2BSscFactors$symFactor_perc <=2, 
                       exclude = NA, 
                       dnn = c("Diagnosis", "At or Below 2nd Percentile")),
                 margin = 1),2)

round(prop.table(table(esac2BSscFactors$StudyDX, 
                       esac2BSscFactors$symFactor_perc<=5, 
                       exclude = NA, 
                       dnn = c("Diagnosis", "At or Below 5th Percentile")),
                 margin = 1),2)

round(prop.table(table(esac2BSscFactors$StudyDX, 
                       esac2BSscFactors$symFactor_perc<=9, 
                       exclude = NA, 
                       dnn = c("Diagnosis", "At or Below 9th Percentile")),
                 margin = 1),2)


################################################################################
#### merge file with assessment data########

# bring in the file with the assessment data
assessments  <-  ""
ESAC2sample <- read_excel(assessments, guess_max = 5000, na = "NULL!")
as.data.frame(colnames(ESAC2sample))
View(ESAC2sample)

# drop irrelevant variables (i.e., item data, overlapping variables)
data.frame(colnames(ESAC2sample))
esac2sample <- subset(ESAC2sample,select=-c(5:63,128:129))
esac2sample
esac2merge <- as.data.frame(esac2sample)
str(esac2merge)

# manipulate variables
esac2merge$DX_FINALsimple <- as.factor(esac2merge$DX_FINALsimple)
esac2merge$mselsubrisk <- as.factor(esac2merge$mselsubrisk)
esac2merge$BSnormsgroup <- as.factor(esac2merge$BSnormsgroup)
esac2merge$BS_EmotGazeConcern <- as.factor(esac2merge$BS_EmotGazeConcern)
esac2merge$BS_CommConcern <- as.factor(esac2merge$BS_CommConcern)
esac2merge$BS_GestureConcern <- as.factor(esac2merge$BS_GestureConcern)
esac2merge$BS_SoundConcern <- as.factor(esac2merge$BS_SoundConcern)
esac2merge$BS_WordsConcern <- as.factor(esac2merge$BS_WordsConcern)
esac2merge$BS_UnderConcern <- as.factor(esac2merge$BS_UnderConcern)
esac2merge$BS_ObjectsConcern <- as.factor(esac2merge$BS_ObjectsConcern)
esac2merge$BS_SocialConcern <- as.factor(esac2merge$BS_SocialConcern)
esac2merge$BS_SpeechConcern <- as.factor(esac2merge$BS_SpeechConcern)
esac2merge$BS_SymbolicConcern <- as.factor(esac2merge$BS_SymbolicConcern)
esac2merge$BS_TotalConcern <- as.factor(esac2merge$BS_TotalConcern)
str(esac2merge)

## merge data sets
str(esac2merge)
esac2BSscFactorsALL <- merge(esac2BSscFactors,esac2merge,by.x="ChildKey", 
                           by.y="ChildKey",all.x = T, all.y = T)
str(esac2BSscFactorsALL)

data.frame(colnames(esac2BSscFactorsALL))

nrow(esac2BSscFactorsALL)
cor.ci(esac2BSscFactorsALL[,c(42,44,101,104,107,110)], 
       method = "pearson", plot=F)

allcors <- cor(esac2BSscFactorsALL[,c(42,44,101,104,107,110)])
cor.ci(allcors,n=908)

# correlations between the social interaction standardized factor scores and 
## BS scoresfor the BS risk groups data

SI_fact_BScorrs  <-  esac2BSscFactorsALL %>%
  group_by(esac2BSscFactorsALL$bstd_vs_bsrisk) %>%
  summarize(SI_gaze=cor(socintFactor_std,BS_EmotionGazeSS),
            SI_comm=cor(socintFactor_std,BS_CommunicationSS),
            SI_gest=cor(socintFactor_std,BS_GesturesSS),
            SI_sounds=cor(socintFactor_std,BS_SoundsSS),
            SI_words=cor(socintFactor_std,BS_WordsSS),
            SI_objecs=cor(socintFactor_std,BS_ObjectsSS),
            SI_undstd=cor(socintFactor_std,BS_UnderstndngSS),
            SI_social=cor(socintFactor_std,BS_SocialSS),
            SI_speech=cor(socintFactor_std,BS_SpeechSS),
            SI_symb=cor(socintFactor_std,BS_SymbolicSS),
            SI_total=cor(socintFactor_std,BS_TotalSS)
            )


esac2BSscFactorsALL %>%
  group_by(esac2BSscFactorsALL$bstd_vs_bsrisk) %>%
  summarize(Sym_gaze=cor(symFactor_std,BS_EmotionGazeSS),
            Sym_comm=cor(symFactor_std,BS_CommunicationSS),
            Sym_gest=cor(symFactor_std,BS_GesturesSS),
            Sym_sounds=cor(symFactor_std,BS_SoundsSS),
            Sym_words=cor(symFactor_std,BS_WordsSS),
            Sym_objecs=cor(symFactor_std,BS_ObjectsSS),
            Sym_undstd=cor(symFactor_std,BS_UnderstndngSS),
            Sym_social=cor(symFactor_std,BS_SocialSS),
            Sym_speech=cor(symFactor_std,BS_SpeechSS),
            Sym_symb=cor(symFactor_std,BS_SymbolicSS),
            Sym_total=cor(symFactor_std,BS_TotalSS)
            )


round(prop.table(table(esac2BSscFactors$BS_Risk, 
                       esac2BSscFactors$socintFactor_perc<=9, 
                       exclude = NA, 
                       dnn = c("BS Risk", "At or Below 9th Percentile")),
                 margin = 1),2)

# correlations between the symbolic standardized factor scores and BS scores
## for the BS risk groups data

esac2BSscFactorsALL %>%
  group_by(esac2BSscFactorsALL$bstd_vs_bsrisk) %>%
  summarize(Sym_gaze=cor(symFactor_std,BS_EmotionGazeSS),
            Sym_comm=cor(symFactor_std,BS_CommunicationSS),
            Sym_gest=cor(symFactor_std,BS_GesturesSS),
            Sym_sounds=cor(symFactor_std,BS_SoundsSS),
            Sym_words=cor(symFactor_std,BS_WordsSS),
            Sym_objecs=cor(symFactor_std,BS_ObjectsSS),
            Sym_undstd=cor(symFactor_std,BS_UnderstndngSS),
            Sym_social=cor(symFactor_std,BS_SocialSS),
            Sym_speech=cor(symFactor_std,BS_SpeechSS),
            Sym_symb=cor(symFactor_std,BS_SymbolicSS),
            Sym_total=cor(symFactor_std,BS_TotalSS)
  )

esac2BSscFactorsALL %>%
  group_by(esac2BSscFactorsALL$bstd_vs_bsrisk) %>%
  summarize(Sym_gaze=cor(symFactor_std,),
            Sym_comm=cor(symFactor_std,BS_CommunicationSS),
            Sym_gest=cor(symFactor_std,BS_GesturesSS),
            Sym_sounds=cor(symFactor_std,BS_SoundsSS),
            Sym_words=cor(symFactor_std,BS_WordsSS),
            Sym_objecs=cor(symFactor_std,BS_ObjectsSS),
            Sym_undstd=cor(symFactor_std,BS_UnderstndngSS),
            Sym_social=cor(symFactor_std,BS_SocialSS),
            Sym_speech=cor(symFactor_std,BS_SpeechSS),
            Sym_symb=cor(symFactor_std,BS_SymbolicSS),
            Sym_total=cor(symFactor_std,BS_TotalSS)
  )


describeBy(esac2BSscFactorsALL$LCSS, group =esac2BSscFactorsALL$StudyDX, 
           data = esac2BSscFactorsALL )

esac2BSscFactorsALL %>%
  group_by(esac2BSscFactorsALL$StudyDX) %>%
  summarize(Sym_gaze=cor(symFactor_std,),
            Sym_comm=cor(symFactor_std,BS_CommunicationSS),
            Sym_gest=cor(symFactor_std,BS_GesturesSS),
            Sym_sounds=cor(symFactor_std,BS_SoundsSS),
            Sym_words=cor(symFactor_std,BS_WordsSS),
            Sym_objecs=cor(symFactor_std,BS_ObjectsSS),
            Sym_undstd=cor(symFactor_std,BS_UnderstndngSS),
            Sym_social=cor(symFactor_std,BS_SocialSS),
            Sym_speech=cor(symFactor_std,BS_SpeechSS),
            Sym_symb=cor(symFactor_std,BS_SymbolicSS),
            Sym_total=cor(symFactor_std,BS_TotalSS)
  )


esac2BSscFactorsALL %>%
  group_by(esac2BSscFactorsALL$DX_FINALsimple) %>%
  summarize(Sym_VRT=cor(socintFactor_std,VRT),
            Sym_FMT=cor(socintFactor_std,FMT),
            Sym_RLT=cor(socintFactor_std,RLT),
            Sym_ELT=cor(socintFactor_std,ELT),
           
           )


