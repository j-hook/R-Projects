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
## This file comprises the syntax for the data pre-process and analyses 
## To address the first two aims of the study. Specifically, 
## this syntax include the upload of the item-level data, group  and age 
## comparisons for the two identified dimensions of the ESAC: 
## Social Factor ("socintFactor_std") and Symbolic Factor ("symFactor_std"). 
## Finally, the two dimension scores were examined in relation to the 
## standardized assessment measures and in exploratory discriminant analysis 
## comparing diagnostic groups. 

# This file does not include all analyses reported in the manuscript.

###############################################################################
library(readxl)
library(psych)
library(caret)
library(pROC)
library(dplyr)
library(paran)
library(polycor)
library(splitstackshape)
library(ggpubr)
library(dunn.test)

# data pre-processing
file_name  <-  ""
ESAC2BS <- read_excel(file_name, guess_max = 5000, na = "NULL!")
View(ESAC2BS)
str(ESAC2BS, list.len=ncol(ESAC2BS))

esac2BS <- as.data.frame(do.call(cbind,ESAC2BS))
str(esac2BS, list.len=ncol(esac2BS))
data.frame(colnames(esac2BS))

esac2BS[,c(1:55,63,64)] <- lapply(esac2BS[,c(1:55,63,64)],as.numeric)
esac2BS$StudyDX <- as.factor(esac2BS$StudyDX)
esac2BS$BSDX <- as.factor(esac2BS$BSDX)
esac2BS$Concern <- as.factor(esac2BS$Concern)
esac2BS$Sex <- as.factor(esac2BS$Sex)
esac2BS$BS_Risk <- as.numeric(esac2BS$BS_Risk)
esac2BS$AgeGroup <- as.numeric(esac2BS$AgeGroup)

# SUBSET INTO Social Communication ITEMS ONLY 
## (Drop items 12,13,20,23,25,27,28,29,33,34,35,36,40,42,43,44,45,46,47)
esac2BSsc <- subset(esac2BS,select=-c(Q12,Q13,Q20,Q23,Q25,Q27,Q28,Q29,Q33,
                                      Q34,Q35,Q36,Q40,Q42,Q43,Q44,Q45,Q46,Q47))
data.frame(colnames(esac2BSsc))
# CHANGE 3 pt ITEMS INTO CATEGORICAL and item 2
esac2BSsc[,c(1:30,32,33,35,36)] <- lapply(esac2BSsc[,c(1:30,32,33,35,36)],
                                          as.ordered)
str(esac2BSsc, list.len=ncol(esac2BSsc))

# examine item response distributions for each item
for (x in 1:36){
  prop.table(table(esac2BS[,x]))
}

# FULL SAMPLE CORRELATION accounting for categorical and continuous 
## structure of the items
require(polycor)
options(max.print = 1000000)
hetcor(esac2BSsc[,c(1:36)],ML=T, use = "pairwise.complete.obs", std.err = F)

# drop item Q21pointsnear due to its large correlation with item Q21pointsfar, 
# re-estiamte the correlation table
esaccorr <- hetcor(esac2BSsc[,c(1:23,25:36)],ML=T, 
                   use = "pairwise.complete.obs", std.err = F)
esaccorr$correlations

# Randomly partition the sample into exploratory (65%) 
## and confirmatory (35%) samples
## controlling for age and diagnostic outcomes (CBE and BS outcome)
set.seed(12897)
efaindex <- stratified(esac2BSsc, c("Age_month","BSDX"), .65, bothSets = T)
efa.esacSC <- efaindex$SAMP1
cfa.esacSC <- efaindex$SAMP2

# write out files as data sets..to be used for mplus
write.csv(efa.esacSC,"")
write.csv(cfa.esacSC,"")

# review sample sizes
nrow(efa.esacSC)
nrow(cfa.esacSC)
nrow(esac2BSsc)

# histograms of age across samples
hist(efa.esacSC$Age_month)
hist(cfa.esacSC$Age_month)
hist(efa.esacSC$ESACage, 
     xlim = c(12,26))
hist(cfa.esacSC$ESACage,
     xlim = c(12,26))

# proportions of samples by BSDX
prop.table(table(esac2BSsc$BSDX))
prop.table(table(efa.esacSC$BSDX))
prop.table(table(cfa.esacSC$BSDX))

# frequency of samples by BSDX
table(esac2BSsc$BSDX)
table(efa.esacSC$BSDX)
table(cfa.esacSC$BSDX)

# proportions of samples by Age_month
prop.table(table(esac2BSsc$Age_month))
prop.table(table(efa.esacSC$Age_month))
prop.table(table(cfa.esacSC$Age_month))

# proportions of samples-BSDX by Age_month
prop.table(table(esac2BSsc$BSDX, esac2BSsc$Age_month))
prop.table(table(efa.esacSC$BSDX, efa.esacSC$Age_month))
prop.table(table(cfa.esacSC$BSDX, cfa.esacSC$Age_month))

# parallel analysis 
# create correlation matrix for parallel analysis
# EFA SAMPLE CORRELATION
data.frame(colnames(efa.esacSC))
options(max.print = 1000000)
efacorr <- hetcor(efa.esacSC[,c(1:23,25:36)],ML=T, 
                  use = "pairwise.complete.obs", std.err = F)

# PARALLEL ANALYSES
library(psych)
library(paran)
fa.parallel(efacorr$correlations,n.obs = 597, fa="both", fm="minres", 
            cor="mixed")
vss(efacorr$correlations, n = 10, rotate = "oblimin", diagonal = TRUE, 
    fm = "minres", n.obs=597, plot=TRUE,use="pairwise",cor="mixed")
nfactors(efacorr$correlations, rotate = "oblimin", fm= "ml",
         n.obs = 597, cor = "mixed")
paran(efacorr$correlations,cfa=T,all = T, graph=T, color=T, 
      col=c("black", "red", "blue"))

# run factor analyses to compare with mplus estimations

onefactor <- fa(esaccorr$correlations, n.obs = 908, nfactors = 1, 
                rotate = "oblimin", fm="wls", cor = "mixed")
onefactor
summary(onefactor)

print(onefactor$loadings,cutoff = 0.3)

twofactor <- fa(esaccorr$correlations,n.obs = 908, nfactors = 2, 
                rotate = "oblimin", fm="wls", cor = "mixed")
summary(twofactor)
factor.plot(twofactor)
print(twofactor$loadings,cutoff = 0.3)

threefactor <- fa(esaccorr$correlations, n.obs = 908, nfactors = 3, 
                  rotate = "oblimin", fm="wls", cor = "mixed")
summary(threefactor)
factor.plot(threefactor)
print(threefactor$loadings,cutfoff=0.3)

######### FULL SAMPLE ANALYSES-based on results of the factor analysis in mplus

# summary of mplus results: 2 factors were ultimately extracted
# factor 1: 'social interaction factor': 
## items Q1,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q14,Q17,Q18,Q19,Q21GIVES,Q21SHOW,Q21PUSH,
#        Q21RAISE,Q21REACH,Q21WAVES,Q21PTFAR,Q22,Q24,Q30,Q31,Q41
# factor 2: 'symbolic factor': 
## items Q21SHAKE,Q21NODS,Q37,Q38,Q39
# items dropped from analysis: Q2, Q3, Q4, Q15, Q16, Q21pointsnear, Q32

# create row sums for each factor
str(esac2BSsc)
data.frame(colnames(esac2BSsc))
# drop the excluded items from mplus factor analysis..see above
esacBSscfactoritems <- subset(esac2BSsc, 
                              select = -c(Q2,Q3,Q4,Q15,Q16,Q21pointsnear,Q32))
data.frame(colnames(esacBSscfactoritems))
# change the categorical items back into integers
esacBSscfactoritems[,c(1:29)] <- lapply(esacBSscfactoritems[,c(1:29)], as.numeric)

# recode the variables into reverse order such that larger values 
## are indicative of more "skill"

esac2BSscrecode <- esacBSscfactoritems
data.frame(colnames(esacBSscfactoritems))
esac2BSscrecode[,c(1:24,26,28,29)] <- 
  ifelse(esac2BSscrecode[,c(1:24,26,28,29)]==0,2,
         ifelse(esac2BSscrecode[,c(1:24,26,28,29)]==1,1,
                ifelse(esac2BSscrecode[,c(1:24,26,28,29)]==2,0,"NA")))
# recode continuous items
esac2BSscrecode[,25] <- 12-esac2BSscrecode[,25]
esac2BSscrecode[,27] <- 8-esac2BSscrecode[,27]
esac2BSscrecode[,c(1:29)] <- lapply(esac2BSscrecode[,c(1:29)],as.numeric)
# check to make sure there isn't any missing data
names(which(colSums(is.na(esac2BSscrecode[,c(1:29)]))>0))

# double check that the recoding was done correctly
data.frame(esacBSscfactoritems$Q1,esac2BSscrecode$Q1)
data.frame(esacBSscfactoritems$Q21gives,esac2BSscrecode$Q21gives)
data.frame(esacBSscfactoritems$Q31,esac2BSscrecode$Q31)
data.frame(esacBSscfactoritems$Q38,esac2BSscrecode$Q38)

# create social interaction factor and symbolic factor
esac2BSscrecode$socintfactor <- 
  rowSums(esac2BSscrecode[,c("Q1","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q14",
                             "Q17","Q18","Q19","Q21gives","Q21shows",
                             "Q21pushes","Q21raises","Q21reaches","Q21waves",
                             "Q21pointsfar","Q22","Q24","Q30","Q31","Q41")])
esac2BSscrecode$symbfactor <- 
  rowSums(esac2BSscrecode[,c("Q21shakes","Q21nods","Q37","Q38","Q39")])


# create age groups
age_groups <- 
  ifelse(esac2BSscrecode$ESACage<14.945,"12-14m",
         ifelse(esac2BSscrecode$ESACage>=14.945 & 
                  esac2BSscrecode$ESACage<17.945,"15-17m",
                ifelse(esac2BSscrecode$ESACage>=17.945 & 
                         esac2BSscrecode$ESACage<20.945,"18-20m",
                       ifelse(esac2BSscrecode$ESACage>=20.945,"21-25m","NA"))))
as.factor(age_groups)
esac2BSscrecode$age_groups <- age_groups

# double check
describeBy(esac2BSscrecode$ESACage,group=esac2BSscrecode$age_groups)

# extract this file
write.csv(esac2BSscrecode,"")

# Descriptive frequencies of age groups by BS age groups by CBE groups.
table(esac2BSscrecode$age_groups,esac2BSscrecode$StudyDX)

# drop ASD & DD from the data set
data.frame(colnames(esac2BSscrecode))
esac2BSscrec_noASDDD <- subset(esac2BSscrecode, 
                               StudyDX == "TD" | StudyDX == "NDO", 
                               select = c(1:41))
# count of age groups by bs risk classification (with ASD or DD)
with(esac2BSscrec_noASDDD,
     table(age_groups,BS_Risk))

# drop BS risk in addition to ASD & DD
esac2bsscrec_BSTD <- subset(esac2BSscrec_noASDDD, BS_Risk==0, select = c(1:41))
# count across age groups
table(esac2bsscrec_BSTD$age_groups)

##.....BS-TD group ONLY....no BS-risk, no ASD, no DD....#######################

# histogram of factors
# social interaction factor
describe(esac2bsscrec_BSTD$socintfactor)
hist(esac2bsscrec_BSTD$socintfactor, xlab = "social interaction factor", 
     col = "# 339999",
     main = "Historgram of social interaction factor",
     breaks = "FD",
     xlim=c(10,70))
axis(side=1,at=seq(10,70,10), labels = seq(14,70,10))

# symbolic factor
describe(esac2bsscrec_BSTD$symbfactor)
hist(esac2bsscrec_BSTD$symbfactor, xlab = "symbolic factor", col = "# 9966CC",
     main = "Histogram of symbolic factor",
     breaks = 16,
     xlim = c(0,16))
axis(side=1,at=seq(0,16,4), labels = seq(0,16,4))

# plot factor scores by age
# social interaction factor
plot(esac2bsscrec_BSTD$ESACage,esac2bsscrec_BSTD$socintfactor, 
     pch=21, bg = "gray52",
     xlab = "ESAC age",
     ylab = "Social Interaction Factor Score",
     main = "Scatter plot of Social Interaction by Age",
     xlim = c(min(esac2bsscrec_BSTD$ESACage),max(esac2bsscrec_BSTD$ESACage)),
     ylim = c(min(esac2bsscrec_BSTD$socintfactor),
              max(esac2bsscrec_BSTD$socintfactor)))
abline(lm(esac2bsscrec_BSTD$socintfactor~esac2bsscrec_BSTD$ESACage), 
       col="green",lwd=3)
lines(lowess(esac2bsscrec_BSTD$socintfactor~esac2bsscrec_BSTD$ESACage), 
      col="blue", lwd=3)
legend("bottomright", legend=c("Linear", "Smooth"),
        lwd=3, lty=c(2,1,1), col=c("green","blue"))
text(paste("Correlation:", round(cor(esac2bsscrec_BSTD$ESACage,
                                     esac2bsscrec_BSTD$socintfactor),2)), 
     x=20, y=20)

dev.off()
# symbolic factor
plot(esac2bsscrec_BSTD$ESACage,esac2bsscrec_BSTD$symbfactor,pch=21, bg="gray52",
     xlab = "ESAC age",
     ylab = "Symbolic Factor Score",
     main = "Scatter plot of Symbolic by Age",
     xlim = c(min(esac2bsscrec_BSTD$ESACage),
              max(esac2bsscrec_BSTD$ESACage)),
     ylim = c(min(esac2bsscrec_BSTD$symbfactor),
              max(esac2bsscrec_BSTD$symbfactor)))
abline(lm(esac2bsscrec_BSTD$symbfactor~esac2bsscrec_BSTD$ESACage), 
       col= "green", lwd=3)
lines(lowess(esac2bsscrec_BSTD$symbfactor~esac2bsscrec_BSTD$ESACage), 
      col = "blue", lwd=3)
legend("topleft", legend = c("Linear", "Smooth"),
       lwd=3,lty=c(2,1,1), col=c("green", "blue"))
text(paste("Correlation:", 
           round(cor(esac2bsscrec_BSTD$ESACage,esac2bsscrec_BSTD$symbfactor),2)), 
     x=13, y=14)

# # density plots by age group
library(ggplot2)
colrs <- c('#7b3294','#c2a5cf','#a6dba0','#008837')

# social interaction 
ggplot(esac2bsscrec_BSTD, aes(x=socintfactor, colour=age_groups))+
  geom_density(lwd=1.2,linetype=1)+
  scale_color_manual(values=colrs)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title = "Social Interaction Density Curve by Age Group",
       x="Social Interaction Score",y="Density")
dev.off()
# symbolic
ggplot(esac2bsscrec_BSTD,aes(x=symbfactor, colour=age_groups))+
  geom_density(lwd=1.2,linetype=1)+
  scale_color_manual(values=colrs)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title="Symbolic Density Curve by Age Group",
       x="Symbolic Score", y="Density")+
  dev.off()

update_geom_defaults("point", list(colour = "black"))
## Boxplots of raw scores by age groups
# social interaction

boxcolrs <- c('#edf8fb','#b2e2e2','#66c2a4','#238b45')
SIplot <- ggplot(esac2bsscrec_BSTD,aes(x=age_groups,y=socintfactor))+
  geom_boxplot(aes(fill=age_groups))+
  stat_boxplot(geom = 'errorbar')+
  scale_fill_manual(values = boxcolrs)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12), 
        axis.text.y = element_text(size=9),
        plot.title = element_text(hjust = 0.5, size=12), 
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        plot.margin = unit(c(0.25,1,0.25,1),"cm"))+
  labs(title="Social Interaction Factor by Age Group",
        y="Social Interaction Factor Raw Score",
       color="Age Group", fill="Age Group")


# symbolic
symplot <- ggplot(esac2bsscrec_BSTD,aes(x=age_groups,y=symbfactor))+
  geom_boxplot(aes(fill=age_groups))+
  stat_boxplot(geom = 'errorbar')+
  scale_fill_manual(values = boxcolrs)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12), 
        axis.text.y =  element_text(size=9),
        plot.title = element_text(hjust = 0.5, size=12), 
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        plot.margin = unit(c(0.25,1,0.25,1),"cm"), 
        plot.background = element_blank())+
  labs(title = "Symbolic Factor by Age Group",
        y = "Symbolic Factor Raw Score",
       color= "Age Group", fill="Age Group")


figure1 <- ggarrange(SIplot, symplot, labels = c("A", "B"), 
                   ncol = 2, nrow = 1, common.legend = TRUE,legend="bottom")+
          theme(plot.margin = unit(c(0.5,0,0.5,0.25),"cm"), 
                plot.background = element_blank())
figure1
annotate_figure(figure1, top = text_grob('Boxplots of Factors by Age Group', 
                                         color = "black", 
                                         size = 13, face = "bold"),
                fig.lab = "Figure 1", fig.lab.size = 14, fig.lab.face = "bold")

dev.off()

# Age group comparison across raw scores 
kruskal.test(socintfactor~age_groups, data = esac2bsscrec_BSTD)
dunn.test(esac2bsscrec_BSTD$socintfactor,esac2bsscrec_BSTD$age_groups, 
          method="bonferroni")
# effect size--epsilon squared
h1 <- unname(kruskal.test(socintfactor~age_groups, 
                          data = esac2bsscrec_BSTD)$statistic)
h1
n1 <- sum(table(esac2bsscrec_BSTD$socintfactor,esac2bsscrec_BSTD$age_groups))
n1
epsilon1 <- h1*(n1+1)/(n1^2-1)
epsilon1
# effect size--eta-squared
k1 <- length(unique(esac2bsscrec_BSTD$age_groups))
k1
eta1 <- (h1-k1+1)/(n1-k1)
eta1
kruskal_effsize(socintfactor~age_groups, data = esac2bsscrec_BSTD)

# symbolic factor
kruskal.test(symbfactor~age_groups, data = esac2bsscrec_BSTD)
dunn.test(esac2bsscrec_BSTD$symbfactor,esac2bsscrec_BSTD$age_groups, 
          method="bonferroni")
# effect size--eta-squared
kruskal_effsize(symbfactor~age_groups, data = esac2bsscrec_BSTD)
# effect size--epsilon squared
h2 <- unname(kruskal.test(symbfactor~age_groups, 
                          data = esac2bsscrec_BSTD)$statistic)
h2
n2 <- sum(table(esac2bsscrec_BSTD$symbfactor,esac2bsscrec_BSTD$age_groups))
n2
epsilon2 <- h2*(n2+1)/(n2^2-1)
epsilon2

#######create z scores for the BS-TD sample------NOT USED FOR MANUSCRIPT########
# social interaction factor z-score
esac2bsscrec_BSTD$socint_z <- (esac2bsscrec_BSTD$socintfactor-
                                 mean(esac2bsscrec_BSTD$socintfactor))/sd(
                                   esac2bsscrec_BSTD$socintfactor)
describe(esac2bsscrec_BSTD$socint_z)
# symbolic factor z-score
esac2bsscrec_BSTD$symbolic_z <- (esac2bsscrec_BSTD$symbfactor-
                                   mean(esac2bsscrec_BSTD$symbfactor))/sd(
                                     esac2bsscrec_BSTD$symbfactor)
describe(esac2bsscrec_BSTD$symbolic_z)

# plot histogram of z-scores
# social interaction factor
hist(esac2bsscrec_BSTD$socint_z, xlab = "social interaction z-score", 
     col = "#339999",
     main = "Historgram of social interaction z-score",
     breaks = 15,
     xlim=c(-4.5,4.5))
axis(side=1,at=seq(-4.5,4.5,1.5), labels = seq(-4.5,4.5,1.5))

# symbolic factor
hist(esac2bsscrec_BSTD$symbolic_z,
     xlab = "symbolic z-score",
     col = "#9966CC",
     main = "Histogram of symbolic z-score",
     breaks = 10,
     xlim = c(-3,3))
axix(side=1, at=seq(-3,3,0.5), labels=seq(-3,3,0.5))
# descriptive statistics of social interaction z score by age groups
describeBy(esac2bsscrec_BSTD$socint_z, group = esac2bsscrec_BSTD$age_groups, 
           digits = 2)
describeBy(esac2bsscrec_BSTD$symbolic_z,group = esac2bsscrec_BSTD$age_groups,
           digits = 2)
# box plot of z-scores by age groups
# social interaction
ggplot(esac2bsscrec_BSTD,aes(x=age_groups,y=socint_z))+
  geom_boxplot(outlier.color = "red", aes(fill=age_groups))+
  stat_boxplot(geom = 'errorbar')+
  scale_fill_manual(values = colrs)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size=16), axis.text = element_text(size=12),
        plot.title = element_text(size=18))+
  labs(title="Boxplots of Social interaction z-score by Age Group",
       x="age group", y="social interaction z-score")
dev.off()

# symbolic
ggplot(esac2bsscrec_BSTD,aes(x=age_groups,y=symbolic_z))+
  geom_boxplot(outlier.color = "red",aes(fill=age_groups))+
  stat_boxplot(geom = 'errorbar')+
  scale_fill_manual(values = colrs)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size=16), axis.text = element_text(size=12),
        plot.title = element_text(size=18))+
  labs(title = "Boxplots of Symbolic z-score by Age Group",
       x="Age Group", y = "symbolic z-score")
# means plots by age groups
library(ggpubr)
# social interaction
ggline(esac2bsscrec_BSTD,x="age_groups", y="socint_z",
       order = c("12-14m","15-17m","18-20m","21-25m"),
       add = c("mean_sd"),
       ylab = "Social Interaction Z-score",
       xlab = "Age Group", 
       title = "Plot of Social Interaction Means by Age Group",
       color = colrs, size=1.5,
       ylim=c(-2,2),
       font.x=16,font.y=16, font.main=18)
dev.off()
# symbolic
ggline(esac2bsscrec_BSTD, x="age_groups", y="symbolic_z",
       order = c("12-14m","15-17m","18-20m","21-25m"),
       add = "mean_sd",
       ylab = "Symbolic z-score",
       xlab = "Age Group",
       title = "Plot of Symbolic Means by Age Group",
       color = colrs, size = 1.5,
       ylim=c(-2,2),
       font.x=16,font.y=16, font.main=18)
# ANOVAS
library(car)
# social interaction anova
socint_anova <- aov(socint_z~age_groups, data = esac2bsscrec_BSTD)
summary(socint_anova)
coefficients(socint_anova)
# check for homogeneity of variance
plot(socint_anova,1)
# run leveneTest--want non-significant
leveneTest(socint_z~age_groups, data = esac2bsscrec_BSTD, center=mean)
# check for normality of residuals
plot(socint_anova,2)
# run-shapiro-wilk test--want non-significant
socint_anova_resids <- residuals(object = socint_anova)
shapiro.test(x=socint_anova_resids)
## IF ASSUMPTIONS ARE NOT VIOLATED
TukeyHSD(socint_anova)
pairwise.t.test(esac2bsscrec_BSTD$socint_z,esac2bsscrec_BSTD$age_groups,
                p.adjust.method = "holm")

# non-parametric ANOVA-Kruskal-Wallis Test--ANOVA assumptions were violated
kruskal.test(socint_z~age_groups, data = esac2bsscrec_BSTD)
pairwise.wilcox.test(esac2bsscrec_BSTD$socint_z,esac2bsscrec_BSTD$age_groups,
                     p.adjust.method = "holm")
pairwise.wilcox.test(esac2bsscrec_BSTD$socint_z,esac2bsscrec_BSTD$age_groups,
                     p.adjust.method = "bonferroni")

# symbolic anova
symbolic_anova <- aov(symbolic_z~age_groups, data=esac2bsscrec_BSTD)
summary(symbolic_anova)
# check homogeneity of variance assumption
plot(symbolic_anova,1)
# run leveneTest--want non-significant
leveneTest(symbolic_z~age_groups, data=esac2bsscrec_BSTD, center=mean)
# check for normality of residuals
plot(symbolic_anova,2)
# run-shapiro-wilk test--want non-significant
symbolic_anova_resids <- residuals(object = symbolic_anova)
shapiro.test(x=symbolic_anova_resids)

# non-parametric ANOVA-Kruskal-Wallis Test--ANOVA assumptions were violated
kruskal.test(symbolic_z~age_groups, data=esac2bsscrec_BSTD)
pairwise.wilcox.test(esac2bsscrec_BSTD$symbolic_z, esac2bsscrec_BSTD$age_groups,
                     p.adjust.method = "bonferroni")
pairwise.wilcox.test(esac2bsscrec_BSTD$symbolic_z, esac2bsscrec_BSTD$age_groups,
                     p.adjust.method = "holm")

# bring in the file with the assessment data
file_name <- ""
ESAC2sample <- read_excel(, guess_max = 5000, na = "NULL!")
View(ESAC2sample)

# drop irrelevant variables(Drop items 12,13,20,23,25,27,28,29,33,34,35,
## 36,40,42,43,44,45,46,47)
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

# merge data sets
str(esac2BSsc)
str(esac2merge)
esac2bs_scmerge <- merge(esac2BSscrecode,esac2merge,by.x="ChildKey", 
                         by.y="ChildKey",all.x = T)
str(esac2bs_scmerge)



# plot historgram of factor scores
hist(esac2bs_scmerge$socintfactor, xlab = "social interaction factor", 
     col = "#339999",
     main = "Historgram of social interaction factor")
hist(esac2bs_scmerge$symbfactor, xlab = "symbolic factor", col = "#9966CC",
     main = "Histogram of symbolic factor")

# descriptives
describe(esac2bs_scmerge[,c("socintfactor","symbfactor")])
describeBy(esac2bs_scmerge[,c("socintfactor","symbfactor")],
           group=esac2bs_scmerge$AgeGroup)
describeBy(esac2bs_scmerge[,c("socintfactor","symbfactor")],
           group=esac2bs_scmerge$BS_Risk)
describeBy(esac2bs_scmerge[,c("socintfactor","symbfactor")],
           group=esac2bs_scmerge$StudyDX)


# correlations, create a subset 
data.frame(colnames(esac2bs_scmerge))
esac2fcorrs=subset(esac2bs_scmerge,select = c(44,46,47,81,84,87,90,93,96,99,
                                              102,105,108,111))
cor(esac2fcorrs, method = "pearson")


# roc analysis...whole sample

# social interaction factor
socint_roc <- roc(esac2bs_scmerge$BS_Risk,esac2bs_scmerge$socintfactor,
                  auc=TRUE, ci=TRUE)
socint_roc
data.frame(socint_roc$thresholds,socint_roc$sensitivities,
           socint_roc$specificities)
coords(socint_roc, x = "best", ret = c("threshold",
                                       "sensitivity", "specificity"), 
       best.method = "y")
plot.roc(socint_roc,ci = TRUE, print.auc = T, print.thres = "best")
plot(1:nrow(esac2bs_scmerge), esac2bs_scmerge$socintfactor, 
     col=ifelse(esac2bs_scmerge$BS_Risk==1, "red", "black"))

# social interaction factor
symb_roc <- roc(esac2bs_scmerge$BS_Risk,esac2bs_scmerge$symbfactor,
                auc=TRUE, ci=TRUE)
symb_roc
data.frame(symb_roc$thresholds,symb_roc$sensitivities,symb_roc$specificities)
coords(symb_roc, x = "best", ret = c("threshold", "sensitivity", "specificity"), 
       best.method = "y")
plot.roc(symb_roc,ci = TRUE, print.auc = T, print.thres = "best")
plot(1:nrow(esac2bs_scmerge), esac2bs_scmerge$symbfactor, 
     col=ifelse(esac2bs_scmerge$BS_Risk==1, "red", "black"))
# split into younger and older groups
young.esac.sc <- subset(esac2bs_scmerge, AgeGroup==1)
nrow(young.esac.sc)
older.esac.sc <- subset(esac2bs_scmerge, AgeGroup==2)
nrow(older.esac.sc)

# younger plots
library(ggplot2)
ggplot(young.esac.sc,aes(x=young.esac.sc$socintfactor))+
  geom_histogram(aes(color=young.esac.sc$BS_Risk),
                 position = "identity", bins=40)+
  scale_color_manual(values=c("339999","9966CC"))
hist(young.esac.sc$socintfactor, xlab = "social interaction factor", 
     col = "#339999",
     main = "Historgram of social interaction factor-younger")
hist(youngBS$symbfactor, xlab = "symbolic factor", col = "#9966CC",
     main = "Histogram of symbolic factor")

# younger sample
# social interaction factor
youngsocint_roc <- roc(young.esac.sc$BS_Risk,young.esac.sc$socintfactor,
                       auc=TRUE, ci=TRUE)
youngsocint_roc
data.frame(youngsocint_roc$thresholds,youngsocint_roc$sensitivities,
           youngsocint_roc$specificities)
coords(youngsocint_roc, x = "best", ret = c("threshold", "sensitivity", 
                                            "specificity"), 
       best.method = "y")
plot.roc(youngsocint_roc,ci = TRUE, print.auc = T, print.thres = "best")


# social interaction factor
youngsymb_roc <- roc(young.esac.sc$BS_Risk,young.esac.sc$symbfactor,
                     auc=TRUE, ci=TRUE)
youngsymb_roc
data.frame(youngsymb_roc$thresholds,youngsymb_roc$sensitivities,
           youngsymb_roc$specificities)
coords(youngsymb_roc, x = "best", ret = c("threshold", "sensitivity", 
                                          "specificity"), 
       best.method = "y")
plot.roc(youngsymb_roc,ci = TRUE, print.auc = T, print.thres = "best")

# older sample
# social interaction factor
oldersocint_roc <- roc(older.esac.sc$BS_Risk,older.esac.sc$socintfactor,
                       auc=TRUE, ci=TRUE)
oldersocint_roc
data.frame(oldersocint_roc$thresholds,oldersocint_roc$sensitivities,
           oldersocint_roc$specificities)
coords(oldersocint_roc, x = "best", ret = c("threshold", "sensitivity", 
                                            "specificity"), 
       best.method = "y")
plot.roc(oldersocint_roc,ci = TRUE, print.auc = T, print.thres = "best")

# social interaction factor
oldersymb_roc <- roc(older.esac.sc$BS_Risk,older.esac.sc$symbfactor,
                     auc=TRUE, ci=TRUE)
oldersymb_roc
data.frame(oldersymb_roc$thresholds,oldersymb_roc$sensitivities,
           oldersymb_roc$specificities)
coords(oldersymb_roc, x = "best", ret = c("threshold", "sensitivity", 
                                          "specificity"), 
       best.method = "y")
plot.roc(oldersymb_roc,ci = TRUE, print.auc = T, print.thres = "best")

a <- aov(esac2bs_scmerge$socintfactor~esac2bs_scmerge$BS_Risk)
summary(a)

noasd.esac.sc <- subset(esac2bs_scmerge, StudyDX!="ASD")

describeBy(noasd.esac.sc$socintfactor, group = noasd.esac.sc$Age_month)
