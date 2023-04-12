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
## This file comprises the syntax for the the linear mixed modeling described 
## in the manuscript.
## The outcome variables are each of the cluster and composite scores on the
## main study measure, the CSBS: EmotionGazeWRS, CommunicationWRS, 
## GesturesWRS, SoundsWRS, UnderstandingWRS, ObjectsWRS, SocialWRS, SymbolicWRS, 
## SpeechWRS TotalWRS 
## The predictor variabales are diagnostic group, within-effect of age, 
## between effect of age. We compared two models of interest: a model with
## an interaction between the diagnostic group and the within-effect of age and 
## one without the interaction. We estimated models with random intercepts.


# libraries
library(nlme)
library(ggplot2)
library(readxl)
library(dplyr)

# load in data
longfile  =  "C:/Users/hooke/Desktop/BS/Early-Late2y/bslong.csv"
bsLong <- read.csv(longfile)
as.data.frame(colnames(bsLong))
bsLong$time <- as.factor(bsLong$time)
bsLong$DX <- as.factor(bsLong$DX)
bsLong$ChildKey <- as.factor(bsLong$ChildKey)

# create age_within and age_between variables
baselineage<-subset(bsLong,time==1,select=c(1,4))
colnames(baselineage)[2]="baselineage"
bsLong<-merge(x=bsLong,y=baselineage, by= "ChildKey", all.x = T )
bsLong$age_withinBaseCentered<-bsLong$BSCAge - bsLong$baselineage
bsLong$age_betwnBaseCentered<-bsLong$baselineage - mean(bsLong$BSCAge[bsLong$time==1])

# Emotion & Eye Gaze ------------------------------------------------------

# Comparison 1: full model with interaction of within-child age by diagnosis
# controlling for baseline age vs reduced model without an interaction
full<-lme(EmotionGazeWRS~DX*age_withinBaseCentered+age_betwnBaseCentered , 
          data = bsLong, random = ~1 |ChildKey, method = "ML")
anova(full, type = "marginal")
summary(full)
reduced<-lme(EmotionGazeWRS~DX+age_withinBaseCentered+age_betwnBaseCentered, 
             data = bsLong, random=~1 |ChildKey, method = "ML")
anova(reduced, type = "marginal")
summary(reduced)

# compare model fits
anova(full ,reduced)


# final model
eeg.lme.final <- lme(EmotionGazeWRS~DX+age_withinBaseCentered+
                       age_betwnBaseCentered, data = bsLong, 
                     random=~1 |ChildKey, method = "ML")

plot(eeg.lme.final)
qqnorm(resid(eeg.lme.final))

# create new dataframe for prediction
eeg.newdat <- data.frame(ChildKey = c(25001,25001,26001,26001,27001,27001),
                     DX = c("ASD","ASD","DD","DD","TD","TD"),
                     Age= c(15,21,15,21,15,21),
                     age_withinBaseCentered=c(0,6,0,6,0,6),
                     age_betwnBaseCentered=
                       c((15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                         (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                         (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                         (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                         (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                         (15- mean(bsLong2$BSCAge[bsLong2$time==1]))
                     ))

eeg.newdat$predicted_score <- predict(eeg.lme.final, eeg.newdat, level = 0)

Designmat <- model.matrix(eval(eval(eeg.lme.final$call$fixed)[-2]), 
                          eeg.newdat[-ncol(eeg.newdat)])
predvar <- diag(Designmat %*% eeg.lme.final$varFix %*% t(Designmat))
eeg.newdat$SE <- sqrt(predvar)
eeg.newdat$SElower<-eeg.newdat$predicted_score-eeg.newdat$SE
eeg.newdat$SEupper<-eeg.newdat$predicted_score+eeg.newdat$SE   
eeg.newdat$SE2 <- sqrt(predvar+eeg.lme.final$sigma^2) # "predicted subject score SD"
eeg.newdat


# plot the new data
# get a random sample of data from the raw data to include in the plot
set.seed(19912)
bsdata_subset <- bsdata %>% group_by(StudyDX_final) %>% slice_sample(n=15)
bsdata_subset$ChildKey <- sub("^0+", "", bsdata_subset$ChildKey )
bsdata_subset$ChildKey <- as.factor(bsdata_subset$ChildKey)
bsLong_subset<-bsLong %>%
  filter(ChildKey %in% bsdata_subset$ChildKey)

#plot predicted mean scores data
ggplot()+
  #plot predicted data
  geom_line(data=eeg.newdat, aes(x=(Age), y=predicted_score, group=DX, 
                                 color=DX), linewidth=1.5)+
  theme_classic()+
  geom_errorbar(data=eeg.newdat, aes(x=(age_withinBaseCentered + 15), 
                                     y=predicted_score,ymin=SElower,
                                     ymax=SEupper, color = DX), 
                width=.9, position=position_dodge(0.05), size=1.5)+
  #plot the raw data subset
  geom_line(data= bsLong_subset, aes(y=EmotionGazeWRS, x=BSCAge,group= ChildKey,
                                     color=DX), size=.7, alpha=0.5)+
  labs(title= "Predicted vs Raw Scores (Subsample)", x = 'Child Age (months)', 
       y = "Emotion and Eye Gaze",color = "Diagnosis", fill = "Diagnosis",
       caption = "Error bars represent standard error")+
  scale_x_continuous(breaks=c(12,15,18,21,24))+
  theme(legend.position = "bottom")


# Communication -----------------------------------------------------------

# Comparison 1: full model with interaction of within-child age by diagnosis
# controlling for baseline age vs reduced model without an interaction
full <- lme(CommunicationWRS~DX*age_withinBaseCentered+age_betwnBaseCentered , 
          data = bsLong, random=~1 |ChildKey, method = "ML")
anova(full, type = "marginal")
summary(full)
reduced <- lme(CommunicationWRS~DX+age_withinBaseCentered+age_betwnBaseCentered, 
             data = bsLong, random=~1 |ChildKey, method = "ML")
anova(reduced, type = "marginal")
summary(reduced)

# compare model fits
anova(full ,reduced)


# final model
comm.lme.final <- lme(CommunicationWRS~DX+age_withinBaseCentered+
                       age_betwnBaseCentered, data = bsLong, 
                     random=~1 |ChildKey, method = "ML")

plot(comm.lme.final)
qqnorm(resid(comm.lme.final))

# create new dataframe for prediction
comm.newdat <- data.frame(ChildKey = c(25001,25001,26001,26001,27001,27001),
                         DX = c("ASD","ASD","DD","DD","TD","TD"),
                         Age= c(15,21,15,21,15,21),
                         age_withinBaseCentered=c(0,6,0,6,0,6),
                         age_betwnBaseCentered=
                           c((15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                             (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                             (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                             (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                             (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                             (15- mean(bsLong2$BSCAge[bsLong2$time==1]))
                           ))

comm.newdat$predicted_score <- predict(comm.lme.final, comm.newdat, level = 0)

Designmat <- model.matrix(eval(eval(comm.lme.final$call$fixed)[-2]), 
                          comm.newdat[-ncol(comm.newdat)])
predvar <- diag(Designmat %*% comm.lme.final$varFix %*% t(Designmat))
comm.newdat$SE <- sqrt(predvar)
comm.newdat$SElower <- comm.newdat$predicted_score-comm.newdat$SE
comm.newdat$SEupper <- comm.newdat$predicted_score+comm.newdat$SE   
comm.newdat$SE2 <- sqrt(predvar+comm.lme.final$sigma^2)
comm.newdat 

# plot the new data using subset of data created in Emotion & Eye Gaze above
ggplot()+
  #plot predicted data
  geom_line(data=comm.newdat, aes(x=(Age), y=predicted_score, group=DX, 
                                  color=DX), linewidth=1.5)+
  theme_classic()+
  geom_errorbar(data=comm.newdat, aes(x=(age_withinBaseCentered + 15), 
                                      y=predicted_score,ymin=SElower,
                                      ymax=SEupper, color = DX), 
                width=.9, position=position_dodge(0.05), size=1.5)+
  #plot the raw data subset
  geom_line(data= bsLong_subset, aes(y=CommunicationWRS, x=BSCAge,group= ChildKey,
                                     color=DX), size=.7, alpha=0.5)+
  labs(title= "Predicted vs Raw Scores (Subsample)", x = 'Child Age (months)', 
       y = "Communication",color = "Diagnosis", fill = "Diagnosis",
       caption = "Error bars represent standard error")+
  scale_x_continuous(breaks=c(12,15,18,21,24))+
  theme(legend.position = "bottom")

# Gestures ----------------------------------------------------------------

# Comparison 1: full model with interaction of within-child age by diagnosis
# controlling for baseline age vs reduced model without an interaction
full<-lme(GesturesWRS~DX*age_withinBaseCentered+age_betwnBaseCentered , 
          data = bsLong, random = ~1 |ChildKey, method = "ML")
anova(full, type = "marginal")
summary(full)
reduced<-lme(GesturesWRS~DX+age_withinBaseCentered+age_betwnBaseCentered, 
             data = bsLong, random=~1 |ChildKey, method = "ML")
anova(reduced, type = "marginal")
summary(reduced)

# compare model fits
anova(full ,reduced)

# final model
gest.lme.final <- lme(GesturesWRS~DX+age_withinBaseCentered+
                        age_betwnBaseCentered, data = bsLong, 
                      random=~1 |ChildKey, method = "ML")

plot(gest.lme.final)
qqnorm(resid(gest.lme.final))

# create new dataframe for prediction
gest.newdat <- data.frame(ChildKey = c(25001,25001,26001,26001,27001,27001),
                          DX = c("ASD","ASD","DD","DD","TD","TD"),
                          Age= c(15,21,15,21,15,21),
                          age_withinBaseCentered=c(0,6,0,6,0,6),
                          age_betwnBaseCentered=
                            c((15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1]))
                            ))

gest.newdat$predicted_score <- predict(gest.lme.final, gest.newdat, level = 0)

Designmat <- model.matrix(eval(eval(gest.lme.final$call$fixed)[-2]), 
                          gest.newdat[-ncol(gest.newdat)])
predvar <- diag(Designmat %*% gest.lme.final$varFix %*% t(Designmat))
gest.newdat$SE <- sqrt(predvar)
gest.newdat$SElower<-gest.newdat$predicted_score-gest.newdat$SE
gest.newdat$SEupper<-gest.newdat$predicted_score+gest.newdat$SE   
gest.newdat$SE2 <- sqrt(predvar+gest.lme.final$sigma^2)
gest.newdat


# plot the new data using subset of data created in Emotion & Eye Gaze above
ggplot()+
  #plot predicted data
  geom_line(data=gest.newdat, aes(x=(Age), y=predicted_score, group=DX, 
                                  color=DX), linewidth=1.5)+
  theme_classic()+
  geom_errorbar(data=gest.newdat, aes(x=(age_withinBaseCentered + 15), 
                                      y=predicted_score,ymin=SElower,
                                      ymax=SEupper, color = DX), 
                width=.9, position=position_dodge(0.05), size=1.5)+
  #plot the raw data subset
  geom_line(data= bsLong_subset, aes(y=GesturesWRS, x=BSCAge,group= ChildKey,
                                     color=DX), size=.7, alpha=0.5)+
  labs(title= "Predicted vs Raw Scores (Subsample)", x = 'Child Age (months)', 
       y = "Gestures",color = "Diagnosis", fill = "Diagnosis",
       caption = "Error bars represent standard error")+
  scale_x_continuous(breaks=c(12,15,18,21,24))+
  theme(legend.position = "bottom")


# Sounds ------------------------------------------------------------------

# Comparison 1: full model with interaction of within-child age by diagnosis
# controlling for baseline age vs reduced model without an interaction
full<-lme(SoundsWRS~DX*age_withinBaseCentered+age_betwnBaseCentered , 
          data = bsLong, random = ~1 |ChildKey, method = "ML")
anova(full, type = "marginal")
summary(full)
reduced<-lme(SoundsWRS~DX+age_withinBaseCentered+age_betwnBaseCentered, 
             data = bsLong, random=~1 |ChildKey, method = "ML")
anova(reduced, type = "marginal")
summary(reduced)

# compare model fits
anova(full ,reduced)

# final model
snds.lme.final <- lme(SoundsWRS~DX*age_withinBaseCentered+
                        age_betwnBaseCentered, data = bsLong, 
                      random=~1 |ChildKey, method = "ML")

plot(snds.lme.final)
qqnorm(resid(snds.lme.final))

# create new dataframe for prediction
snds.newdat <- data.frame(ChildKey = c(25001,25001,26001,26001,27001,27001),
                          DX = c("ASD","ASD","DD","DD","TD","TD"),
                          Age= c(15,21,15,21,15,21),
                          age_withinBaseCentered=c(0,6,0,6,0,6),
                          age_betwnBaseCentered=
                            c((15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1]))
                            ))

snds.newdat$predicted_score <- predict(snds.lme.final, snds.newdat, level = 0)

Designmat <- model.matrix(eval(eval(snds.lme.final$call$fixed)[-2]), 
                          snds.newdat[-ncol(snds.newdat)])
predvar <- diag(Designmat %*% snds.lme.final$varFix %*% t(Designmat))
snds.newdat$SE <- sqrt(predvar)
snds.newdat$SElower<-snds.newdat$predicted_score-snds.newdat$SE
snds.newdat$SEupper<-snds.newdat$predicted_score+snds.newdat$SE   
snds.newdat$SE2 <- sqrt(predvar+snds.lme.final$sigma^2)
snds.newdat

# plot the new data using subset of data created in Emotion & Eye Gaze above
ggplot()+
  #plot predicted data
  geom_line(data=snds.newdat, aes(x=(Age), y=predicted_score, group=DX, 
                                  color=DX), size=1.5)+
  theme_classic()+
  geom_errorbar(data=snds.newdat, aes(x=(age_withinBaseCentered + 15), 
                                      y=predicted_score,ymin=SElower,
                                      ymax=SEupper, color = DX), 
                width=.9, position=position_dodge(0.05), size=1.5)+
  #plot the raw data subset
  geom_line(data= bsLong_subset, aes(y=SoundsWRS, x=BSCAge,group= ChildKey,
                                     color=DX), size=.7, alpha=0.5)+
  labs(title= "Predicted vs Raw Scores (Subsample)", x = 'Child Age (months)', 
       y = "Sounds",color = "Diagnosis", fill = "Diagnosis",
       caption = "Error bars represent standard error")+
  scale_x_continuous(breaks=c(12,15,18,21,24))+
  theme(legend.position = "bottom")

# Understanding -----------------------------------------------------------

# Comparison 1: full model with interaction of within-child age by diagnosis
# controlling for baseline age vs reduced model without an interaction
full<-lme(UnderstandingWRS~DX*age_withinBaseCentered+age_betwnBaseCentered , 
          data = bsLong, random = ~1 |ChildKey, method = "ML")
anova(full, type = "marginal")
summary(full)

reduced<-lme(UnderstandingWRS~DX+age_withinBaseCentered+age_betwnBaseCentered, 
             data = bsLong, random=~1 |ChildKey, method = "ML")
anova(reduced, type = "marginal")
summary(reduced)

# compare model fits
anova(full ,reduced)

# final model
undst.lme.final <- lme(UnderstandingWRS~DX*age_withinBaseCentered+
                        age_betwnBaseCentered, data = bsLong, 
                      random=~1 |ChildKey, method = "ML")

plot(undst.lme.final)
qqnorm(resid(undst.lme.final))

# create new dataframe for prediction
undst.newdat <- data.frame(ChildKey = c(25001,25001,26001,26001,27001,27001),
                          DX = c("ASD","ASD","DD","DD","TD","TD"),
                          Age= c(15,21,15,21,15,21),
                          age_withinBaseCentered=c(0,6,0,6,0,6),
                          age_betwnBaseCentered=
                            c((15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1]))
                            ))

undst.newdat$predicted_score <- predict(undst.lme.final, undst.newdat, level = 0)

Designmat <- model.matrix(eval(eval(undst.lme.final$call$fixed)[-2]), 
                          undst.newdat[-ncol(undst.newdat)])
predvar <- diag(Designmat %*% undst.lme.final$varFix %*% t(Designmat))
undst.newdat$SE <- sqrt(predvar)
undst.newdat$SElower<-undst.newdat$predicted_score-undst.newdat$SE
undst.newdat$SEupper<-undst.newdat$predicted_score+undst.newdat$SE   
undst.newdat$SE2 <- sqrt(predvar+undst.lme.final$sigma^2)
undst.newdat

# plot the new data using subset of data created in Emotion & Eye Gaze above
ggplot()+
  #plot predicted data
  geom_line(data=undst.newdat, aes(x=(Age), y=predicted_score, group=DX, 
                                  color=DX), size=1.5)+
  theme_classic()+
  geom_errorbar(data=undst.newdat, aes(x=(age_withinBaseCentered + 15), 
                                      y=predicted_score,ymin=SElower,
                                      ymax=SEupper, color = DX), 
                width=.9, position=position_dodge(0.05), size=1.5)+
  #plot the raw data subset
  geom_line(data= bsLong_subset, aes(y=UnderstandingWRS, x=BSCAge,group= ChildKey,
                                     color=DX), size=.7, alpha=0.5)+
  labs(title= "Predicted vs Raw Scores (Subsample)", x = 'Child Age (months)', 
       y = "Understanding",color = "Diagnosis", fill = "Diagnosis",
       caption = "Error bars represent standard error")+
  scale_x_continuous(breaks=c(12,15,18,21,24))+
  theme(legend.position = "bottom")


# Object Use ------------------------------------------------------------------
# Comparison 1: full model with interaction of within-child age by diagnosis
# controlling for baseline age vs reduced model without an interaction
full<-lme(ObjectsWRS~DX*age_withinBaseCentered+age_betwnBaseCentered , 
          data = bsLong, random = ~1 |ChildKey, method = "ML")
anova(full, type = "marginal")
summary(full)

reduced<-lme(ObjectsWRS~DX+age_withinBaseCentered+age_betwnBaseCentered, 
             data = bsLong, random=~1 |ChildKey, method = "ML")
anova(reduced, type = "marginal")
summary(reduced)

# compare model fits
anova(full ,reduced)

# final model
objs.lme.final <- lme(ObjectsWRS~DX+age_withinBaseCentered+
                         age_betwnBaseCentered, data = bsLong, 
                       random=~1 |ChildKey, method = "ML")

plot(objs.lme.final)
qqnorm(resid(objs.lme.final))

# create new dataframe for prediction
objs.newdat <- data.frame(ChildKey = c(25001,25001,26001,26001,27001,27001),
                           DX = c("ASD","ASD","DD","DD","TD","TD"),
                           Age= c(15,21,15,21,15,21),
                           age_withinBaseCentered=c(0,6,0,6,0,6),
                           age_betwnBaseCentered=
                             c((15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                               (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                               (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                               (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                               (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                               (15- mean(bsLong2$BSCAge[bsLong2$time==1]))
                             ))

objs.newdat$predicted_score <- predict(objs.lme.final, objs.newdat, level = 0)

Designmat <- model.matrix(eval(eval(objs.lme.final$call$fixed)[-2]), 
                          objs.newdat[-ncol(objs.newdat)])
predvar <- diag(Designmat %*% objs.lme.final$varFix %*% t(Designmat))
objs.newdat$SE <- sqrt(predvar)
objs.newdat$SElower<-objs.newdat$predicted_score-objs.newdat$SE
objs.newdat$SEupper<-objs.newdat$predicted_score+objs.newdat$SE   
objs.newdat$SE2 <- sqrt(predvar+objs.lme.final$sigma^2)
objs.newdat

# plot the new data using subset of data created in Emotion & Eye Gaze above
ggplot()+
  #plot predicted data
  geom_line(data=objs.newdat, aes(x=(Age), y=predicted_score, group=DX, 
                                   color=DX), size=1.5)+
  theme_classic()+
  geom_errorbar(data=objs.newdat, aes(x=(age_withinBaseCentered + 15), 
                                       y=predicted_score,ymin=SElower,
                                       ymax=SEupper, color = DX), 
                width=.9, position=position_dodge(0.05), size=1.5)+
  #plot the raw data subset
  geom_line(data= bsLong_subset, aes(y=ObjectsWRS, x=BSCAge,group= ChildKey,
                                     color=DX), size=.7, alpha=0.5)+
  labs(title= "Predicted vs Raw Scores (Subsample)", x = 'Child Age (months)', 
       y = "Object Use",color = "Diagnosis", fill = "Diagnosis",
       caption = "Error bars represent prediction interval")+
  scale_x_continuous(breaks=c(12,15,18,21,24))+
  theme(legend.position = "bottom")


# Social ------------------------------------------------------------------

# Comparison 1: full model with interaction of within-child age by diagnosis
# controlling for baseline age vs reduced model without an interaction
full<-lme(SocialWRS~DX*age_withinBaseCentered+age_betwnBaseCentered , 
          data = bsLong, random = ~1 |ChildKey, method = "ML")
anova(full, type = "marginal")
summary(full)

reduced<-lme(SocialWRS~DX+age_withinBaseCentered+age_betwnBaseCentered, 
             data = bsLong, random=~1 |ChildKey, method = "ML")
anova(reduced, type = "marginal")
summary(reduced)

# compare model fits
anova(full ,reduced)

# final model
social.lme.final <- lme(SocialWRS~DX+age_withinBaseCentered+
                        age_betwnBaseCentered, data = bsLong, 
                      random=~1 |ChildKey, method = "ML")

plot(social.lme.final)
qqnorm(resid(social.lme.final))

# create new dataframe for prediction
social.newdat <- data.frame(ChildKey = c(25001,25001,26001,26001,27001,27001),
                          DX = c("ASD","ASD","DD","DD","TD","TD"),
                          Age= c(15,21,15,21,15,21),
                          age_withinBaseCentered=c(0,6,0,6,0,6),
                          age_betwnBaseCentered=
                            c((15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                              (15- mean(bsLong2$BSCAge[bsLong2$time==1]))
                            ))

social.newdat$predicted_score <- predict(social.lme.final, social.newdat, level = 0)

Designmat <- model.matrix(eval(eval(social.lme.final$call$fixed)[-2]), 
                          social.newdat[-ncol(social.newdat)])
predvar <- diag(Designmat %*% social.lme.final$varFix %*% t(Designmat))
social.newdat$SE <- sqrt(predvar)
social.newdat$SElower<-social.newdat$predicted_score-social.newdat$SE
social.newdat$SEupper<-social.newdat$predicted_score+social.newdat$SE   
social.newdat$SE2 <- sqrt(predvar+social.lme.final$sigma^2)
social.newdat

# plot the new data using subset of data created in Emotion & Eye Gaze above
ggplot()+
  #plot predicted data
  geom_line(data=social.newdat, aes(x=(Age), y=predicted_score, group=DX, 
                                  color=DX), size=1.5)+
  theme_classic()+
  geom_errorbar(data=social.newdat, aes(x=(age_withinBaseCentered + 15), 
                                      y=predicted_score,ymin=SElower,
                                      ymax=SEupper, color = DX), 
                width=.9, position=position_dodge(0.05), size=1.5)+
  #plot the raw data subset
  geom_line(data= bsLong_subset, aes(y=SocialWRS, x=BSCAge,group= ChildKey,
                                     color=DX), size=.7, alpha=0.5)+
  labs(title= "Predicted vs Raw Scores (Subsample)", x = 'Child Age (months)', 
       y = "Social",color = "Diagnosis", fill = "Diagnosis",
       caption = "Error bars represent standard error")+
  scale_x_continuous(breaks=c(12,15,18,21,24))+
  theme(legend.position = "bottom")

# Speech ------------------------------------------------------------------

# Comparison 1: full model with interaction of within-child age by diagnosis
# controlling for baseline age vs reduced model without an interaction
full<-lme(SpeechWRS~DX*age_withinBaseCentered+age_betwnBaseCentered , 
          data = bsLong, random = ~1 |ChildKey, method = "ML")
anova(full, type = "marginal")
summary(full)

reduced<-lme(SpeechWRS~DX+age_withinBaseCentered+age_betwnBaseCentered, 
             data = bsLong, random=~1 |ChildKey, method = "ML")
anova(reduced, type = "marginal")
summary(reduced)

# compare model fits
anova(full ,reduced)

# final model
Speech.lme.final <- lme(SpeechWRS~DX*age_withinBaseCentered+
                          age_betwnBaseCentered, data = bsLong, 
                        random=~1 |ChildKey, method = "ML")

plot(Speech.lme.final)
qqnorm(resid(Speech.lme.final))

# create new dataframe for prediction
Speech.newdat <- data.frame(ChildKey = c(25001,25001,26001,26001,27001,27001),
                            DX = c("ASD","ASD","DD","DD","TD","TD"),
                            Age= c(15,21,15,21,15,21),
                            age_withinBaseCentered=c(0,6,0,6,0,6),
                            age_betwnBaseCentered=
                              c((15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                                (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                                (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                                (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                                (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                                (15- mean(bsLong2$BSCAge[bsLong2$time==1]))
                              ))

Speech.newdat$predicted_score <- predict(Speech.lme.final, Speech.newdat, level = 0)

Designmat <- model.matrix(eval(eval(Speech.lme.final$call$fixed)[-2]), 
                          Speech.newdat[-ncol(Speech.newdat)])
predvar <- diag(Designmat %*% Speech.lme.final$varFix %*% t(Designmat))
Speech.newdat$SE <- sqrt(predvar) # se of the predicted means
Speech.newdat$SElower<-Speech.newdat$predicted_score-Speech.newdat$SE
Speech.newdat$SEupper<-Speech.newdat$predicted_score+Speech.newdat$SE   
Speech.newdat$SE2 <- sqrt(predvar+Speech.lme.final$sigma^2)
Speech.newdat

# plot the new data using subset of data created in Emotion & Eye Gaze above
ggplot()+
  #plot predicted data
  geom_line(data=Speech.newdat, aes(x=(Age), y=predicted_score, group=DX, 
                                    color=DX), size=1.5)+
  theme_classic()+
  geom_errorbar(data=Speech.newdat, aes(x=(age_withinBaseCentered + 15), 
                                        y=predicted_score,ymin=SElower,
                                        ymax=SEupper, color = DX), 
                width=.9, position=position_dodge(0.05), size=1.5)+
  #plot the raw data subset
  geom_line(data= bsLong_subset, aes(y=SpeechWRS, x=BSCAge,group= ChildKey,
                                     color=DX), size=.7, alpha=0.5)+
  labs(title= "Predicted vs Raw Scores (Subsample)", x = 'Child Age (months)', 
       y = "Speech",color = "Diagnosis", fill = "Diagnosis",
       caption = "Error bars represent standard error")+
  scale_x_continuous(breaks=c(12,15,18,21,24))+
  theme(legend.position = "bottom")


# Symbolic ----------------------------------------------------------------

# Comparison 1: full model with interaction of within-child age by diagnosis
# controlling for baseline age vs reduced model without an interaction
full<-lme(SymbolicWRS~DX*age_withinBaseCentered+age_betwnBaseCentered , 
          data = bsLong, random = ~1 |ChildKey, method = "ML")
anova(full, type = "marginal")
summary(full)

reduced<-lme(SymbolicWRS~DX+age_withinBaseCentered+age_betwnBaseCentered, 
             data = bsLong, random=~1 |ChildKey, method = "ML")
anova(reduced, type = "marginal")
summary(reduced)

# compare model fits
anova(full ,reduced)

# final model
symbolic.lme.final <- lme(SymbolicWRS~DX*age_withinBaseCentered+
                          age_betwnBaseCentered, data = bsLong, 
                        random=~1 |ChildKey, method = "ML")

plot(symbolic.lme.final)
qqnorm(resid(symbolic.lme.final))

# create new dataframe for prediction
symbolic.newdat <- data.frame(ChildKey = c(25001,25001,26001,26001,27001,27001),
                            DX = c("ASD","ASD","DD","DD","TD","TD"),
                            Age= c(15,21,15,21,15,21),
                            age_withinBaseCentered=c(0,6,0,6,0,6),
                            age_betwnBaseCentered=
                              c((15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                                (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                                (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                                (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                                (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                                (15- mean(bsLong2$BSCAge[bsLong2$time==1]))
                              ))

symbolic.newdat$predicted_score <- predict(symbolic.lme.final, symbolic.newdat, level = 0)

Designmat <- model.matrix(eval(eval(symbolic.lme.final$call$fixed)[-2]), 
                          symbolic.newdat[-ncol(symbolic.newdat)])
predvar <- diag(Designmat %*% symbolic.lme.final$varFix %*% t(Designmat))
symbolic.newdat$SE <- sqrt(predvar)
symbolic.newdat$SElower<-symbolic.newdat$predicted_score-symbolic.newdat$SE
symbolic.newdat$SEupper<-symbolic.newdat$predicted_score+symbolic.newdat$SE   
symbolic.newdat$SE2 <- sqrt(predvar+symbolic.lme.final$sigma^2) #"prediction interval SD"
symbolic.newdat

# plot the new data using subset of data created in Emotion & Eye Gaze above
ggplot()+
  #plot predicted data
  geom_line(data=symbolic.newdat, aes(x=(Age), y=predicted_score, group=DX, 
                                    color=DX), size=1.5)+
  theme_classic()+
  geom_errorbar(data=symbolic.newdat, aes(x=(age_withinBaseCentered + 15), 
                                        y=predicted_score,ymin=SElower,
                                        ymax=SEupper, color = DX), 
                width=.9, position=position_dodge(0.05), size=1.5)+
  #plot the raw data subset
  geom_line(data= bsLong_subset, aes(y=SymbolicWRS, x=BSCAge,group= ChildKey,
                                     color=DX), size=.7, alpha=0.5)+
  labs(title= "Predicted vs Raw Scores (Subsample)", x = 'Child Age (months)', 
       y = "Symbolic",color = "Diagnosis", fill = "Diagnosis",
       caption = "Error bars represent standard error")+
  scale_x_continuous(breaks=c(12,15,18,21,24))+
  theme(legend.position = "bottom")


# Total -------------------------------------------------------------------

# Comparison 1: full model with interaction of within-child age by diagnosis
# controlling for baseline age vs reduced model without an interaction
full<-lme(TotalWRS~DX*age_withinBaseCentered+age_betwnBaseCentered , 
          data = bsLong, random = ~1 |ChildKey, method = "ML")
anova(full, type = "marginal")
summary(full)

reduced<-lme(TotalWRS~DX+age_withinBaseCentered+age_betwnBaseCentered, 
             data = bsLong, random=~1 |ChildKey, method = "ML")
anova(reduced, type = "marginal")
summary(reduced)

# compare model fits
anova(full ,reduced)

# final model
total.lme.final <- lme(TotalWRS~DX*age_withinBaseCentered+
                            age_betwnBaseCentered, data = bsLong, 
                          random=~1 |ChildKey, method = "ML")

plot(total.lme.final)
qqnorm(resid(total.lme.final))

# create new dataframe for prediction
total.newdat <- data.frame(ChildKey = c(25001,25001,26001,26001,27001,27001),
                              DX = c("ASD","ASD","DD","DD","TD","TD"),
                              Age= c(15,21,15,21,15,21),
                              age_withinBaseCentered=c(0,6,0,6,0,6),
                              age_betwnBaseCentered=
                                c((15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                                  (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                                  (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                                  (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                                  (15- mean(bsLong2$BSCAge[bsLong2$time==1])),
                                  (15- mean(bsLong2$BSCAge[bsLong2$time==1]))
                                ))

total.newdat$predicted_score <- predict(total.lme.final, total.newdat, level =0)
                                      

Designmat <- model.matrix(eval(eval(total.lme.final$call$fixed)[-2]), 
                          total.newdat[-ncol(total.newdat)])
predvar <- diag(Designmat %*% total.lme.final$varFix %*% t(Designmat))
total.newdat$SE <- sqrt(predvar) #SE for the population predicted estimate
total.newdat$SElower<-total.newdat$predicted_score-total.newdat$SE
total.newdat$SEupper<-total.newdat$predicted_score+total.newdat$SE   
total.newdat$SE2 <- sqrt(predvar+total.lme.final$sigma^2) #supposedly "prediction interval SD")
total.newdat

# plot the new data using subset of data created in Emotion & Eye Gaze above
ggplot()+
  #plot predicted data
  geom_line(data=total.newdat, aes(x=(Age), y=predicted_score, group=DX, 
                                      color=DX), size=1.5)+
  theme_classic()+
  geom_errorbar(data=total.newdat, aes(x=(age_withinBaseCentered + 15), 
                                          y=predicted_score,ymin=SElower,
                                          ymax=SEupper, color = DX), 
                width=.9, position=position_dodge(0.05), size=1.5)+
  #plot the raw data subset
  geom_line(data= bsLong_subset, aes(y=TotalWRS, x=BSCAge,group= ChildKey,
                                     color=DX), size=.7, alpha=0.5)+
  labs(title= "Predicted vs Raw Scores (Subsample)", x = 'Child Age (months)', 
       y = "Total",color = "Diagnosis", fill = "Diagnosis",
       caption = "Error bars represent standard error")+
  scale_x_continuous(breaks=c(12,15,18,21,24))+
  theme(legend.position = "bottom")


library(ggpubr)
#create multi-panel plot of composite scores

# social
p1 <- ggplot()+
  #plot predicted data
  geom_line(data=social.newdat, aes(x=(Age), y=predicted_score, group=DX, 
                                          color=DX), size=1.5)+
  theme_classic()+
  geom_errorbar(data=social.newdat, aes(x=(age_withinBaseCentered + 15), 
                                              y=predicted_score,ymin=SElower,
                                              ymax=SEupper, color = DX), 
                width=.9, position=position_dodge(0.05), size=1.5)+
  #plot the raw data subset
  geom_line(data= bsLong_subset, aes(y=SocialWRS, x=BSCAge,group= ChildKey,
                                     color=DX), size=.7, alpha=0.5)+
  labs(title= "Social", x = 'Child Age (months)', 
       y = "Raw Score",color = "Diagnosis", fill = "Diagnosis",
       )+
  scale_x_continuous(breaks=c(12,15,18,21,24))+
  theme(legend.position = "bottom")+
  coord_cartesian(xlim = c(12, 24), ylim = c(0, 60), expand = FALSE)
p1
# speech
p2 <- ggplot()+
  #plot predicted data
  geom_line(data=Speech.newdat, aes(x=(Age), y=predicted_score, group=DX, 
                                          color=DX), size=1.5)+
  theme_classic()+
  geom_errorbar(data=Speech.newdat, aes(x=(age_withinBaseCentered + 15), 
                                              y=predicted_score,ymin=SElower,
                                              ymax=SEupper, color = DX), 
                width=.9, position=position_dodge(0.05), size=1.5)+
  #plot the raw data subset
  geom_line(data= bsLong_subset, aes(y=SpeechWRS, x=BSCAge,group= ChildKey,
                                     color=DX), size=.7, alpha=0.5)+
  labs(title= "Speech", x = 'Child Age (months)', 
       y = "Raw Score",color = "Diagnosis", fill = "Diagnosis",
       )+
  scale_x_continuous(breaks=c(12,15,18,21,24))+
  theme(legend.position = "bottom")+
  coord_cartesian(xlim = c(12, 24), ylim = c(0, 50), expand = FALSE)
p2
# symbolic
p3 <- ggplot()+
  #plot predicted data
  geom_line(data=symbolic.newdat, aes(x=(Age), y=predicted_score, group=DX, 
                                            color=DX), size=1.5)+
  theme_classic()+
  geom_errorbar(data=symbolic.newdat, aes(x=(age_withinBaseCentered + 15), 
                                                y=predicted_score,ymin=SElower,
                                                ymax=SEupper, color = DX), 
                width=.9, position=position_dodge(0.05), size=1.5)+
  #plot the raw data subset
  geom_line(data= bsLong_subset, aes(y=SymbolicWRS, x=BSCAge,group= ChildKey,
                                     color=DX), size=.7, alpha=0.5)+
  labs(title= "Symbolic", x = 'Child Age (months)', 
       y = "Raw Score",color = "Diagnosis", fill = "Diagnosis",
       )+
  scale_x_continuous(breaks=c(12,15,18,21,24))+
  theme(legend.position = "bottom")+
  coord_cartesian(xlim = c(12, 24), ylim = c(0, 50), expand = FALSE)
p3
# Total 
p4 <- ggplot()+
  #plot predicted data
  geom_line(data=total.newdat, aes(x=(Age), y=predicted_score, group=DX, 
                                         color=DX), size=1.5)+
  theme_classic()+
  geom_errorbar(data=total.newdat, aes(x=(age_withinBaseCentered + 15), 
                                             y=predicted_score,ymin=SElower,
                                             ymax=SEupper, color = DX), 
                width=.9, position=position_dodge(0.05), size=1.5)+
  #plot the raw data subset
  geom_line(data= bsLong_subset, aes(y=TotalWRS, x=BSCAge,group= ChildKey,
                                     color=DX), size=.7, alpha=0.5)+
  labs(title= "Total", x = 'Child Age (months)', 
       y = "Raw Score",color = "Diagnosis", fill = "Diagnosis",
       )+
  scale_x_continuous(breaks=c(12,15,18,21,24))+
  theme(legend.position = "bottom")+
  coord_cartesian(xlim = c(12, 24), ylim = c(0, 150), expand = FALSE)
p4

caption = "Error bars represent standard error"
#put all three plots together into one multipanel plot
multi_plot<- ggarrange(p1,p2,p3,p4, #plots that are going to be included in this multipanel figure
                       labels = c("A", "B", "C","D"), #labels given each panel 
                       ncol = 2, nrow = 2, #adjust plot space 
                       common.legend = T) #does the plot have a common legend
#add titles and labels to the multi-panel graph
multi_plot <- annotate_figure(multi_plot,
                              top = text_grob("Predicted vs Sample Scores", color = "black", face = "bold", size = 11),
                              bottom = text_grob("Error bars represent standard error", size = 10, just = "center") )
multi_plot 


