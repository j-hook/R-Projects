###############################################################################
# This code was utilized for the manuscript:
## Hooker, JL, Day, TN, Parker, E, Slate, EH, Schatschneider, C, Wetherby, AM,
## & Guthrie, W. Using classification trees to characterize the heterogeneity of 
## early social communication in young children with autism spectrum disorder, 
## developmental delay, or typical development (Manuscript in preparation).

# Code written by Jessica Hooker with input from Elizabeth Slate (August 2022)

# Brief description of the data:
## The data set comprised 994 children with an assessment on a social 
## communication measure (BS) between 16-24 months of age,
## diagnostic assessments measures (ADOS, MSEL), 
## demographic measures, and a grouping variable to indicate 
## diagnostic group (CBE:ASD, DD, or TD)

# The primary goal of this analysis was to use supervised machine learning to 
## identify combinations of social communication skills on the BS 
## that best predict diagnostic groups (CBE). We used conditional-CART modeling
## with cross-validation to identify an optimal model. Modeling was compared at 
## the item-level and cluster score-level of the measure. 

# Brief Code Description:
## This file comprises the syntax for the CART modeling with the item-level data
## as well as the cluster-score level data. We first tuned the hyper-parameters 
## of the model by comparing performance across accuracy metrics. We then 
## fit a model to the entire training data set using the identified optimal 
## hyperparameters. Performance of the final model was fit to the held-out test
## data set. After selecting a final model, differences between correctly and 
## incorrectly classified children in the training and test samples were 
## compared across assessment measures. 

# This file does not include all analyses reported in the manuscript.

###############################################################################
# load in libraries
library(readxl)
library(psych)
library(caret)
library(RColorBrewer)
library(effsize)

# to look up cart model information:
names(getModelInfo())
modelLookup("ctree")

# load in the item-level data set
items_file <- ""
bsnew <- read_excel(items_file, guess_max = 5000)
View(bsnew)

# factor group (CBE) and child sex variables
bsnew$Sex <- as.factor(bsnew$Sex)
bsnew$CBE <- as.factor(bsnew$CBE)

# partition the data into a training and test set, stratifying by CBE
## partition size = 80/20

# set random seed
set.seed(16528)

cbe.index <- createDataPartition(bsnew$CBE, p = .8, list=FALSE)
# training data set
train.bs <- bsnew[cbe.index,]
# test data set
test.bs <- bsnew[-cbe.index,]

# compare distribution across diagnostic groups for the whole, training, and 
## test set
str(train.bs, list.len=ncol(train.bs))
table(bsnew$CBE)
prop.table(table(bsnew$CBE))
table(train.bs$CBE)
prop.table(table(train.bs$CBE))
table(test.bs$CBE)
prop.table(table(test.bs$CBE))
train.bs<-as.data.frame(train.bs)
str(train.bs)
str(test.bs)
test.bs<-as.data.frame(test.bs)
testdata<-test.bs[,-1]

# create stratified random folds for cross validation analysis using diagnosis
CVindex <- createFolds(factor(train.bs$CBE), k = 5, returnTrain = T)
# fit the cross-validation parameters
fitControl <- trainControl(index = CVindex,
                         method = "cv",
                         number=5, 
                         classProbs = TRUE,
                         summaryFunction = multiClassSummary)
# create grid for hyperparameters to tune
ctreegrid <- expand.grid(maxdepth=c(1:5),
                       mincriterion= c(0.95, 0.99))
# train the model using Accuracy as the metric
ctreefit1 <- train(CBE~CSBSAge + Sex +  GazeShifts + PosAffect
                 +  GazeFollow + Rate +  BehReg +  SocInt +  JointAtt 
                 +  ConvGest +  DistGest + InvCons + SylCons + InvWords + Words 
                 + InvWordCombo + WordCombo + Understanding + InvActions 
                 + Pretend + Blocks + SeqPlay,
                 data=train.bs,
                 method="ctree2",
                 metric="Accuracy",
                 trControl=fitControl,
                 tuneGrid=ctreegrid)
# review CV results
ctreefit1
# review final model
ctreefit1$finalModel
# visualizatinon of final model
plot(ctreefit1$finalModel)

# plot the performance of each hyperparameter across accuracy metrics
trellis.par.set(caretTheme())
# accuracy
plot(ctreefit1)
# auc
plot(ctreefit1, metric = "AUC")
# kappa
plot(ctreefit1, metric = "Kappa")
# logLoss: For any given problem, a lower log-loss value means better predictions.
# Log Loss is a slight twist on something called the Likelihood Function. 
# In fact, Log Loss is -1 * the log of the likelihood function.
plot(ctreefit1, metric = "logLoss")

# extract the best performing model based on the accuracy metric
acc_model <- ctreefit1$finalModel
# apply model to the training dataset to predict diagnostic outcome & add to dataframe 
acc_model_class <- predict(acc_model,data = train.bs)
train.bs$acc_model_class <- acc_model_class
# apply model to the training dataset to return the exact node the model predicts & add to dataframe
acc_model_node <- predict(acc_model,data = train.bs, type="node")
train.bs$acc_model_node <- acc_model_node

# predict diagnosis from model and compare with actual data set
pClassCBE <- predict(acc_model)
tab.acc <- table(pClassCBE,train.bs$CBE)
# compute accuracy of the model
sum(diag(tab.acc) / sum(tab.acc))

# compare with confusion matrix of predictions
confusionMatrix(train.bs$acc_model_class,train.bs$CBE)

# look at breakdown of which node each child
table(train.bs$CBE,train.bs$acc_model_node)

############# run with AUC as the outcome
ctreefit2 <- train(CBE~CSBSAge + Sex +  GazeShifts + PosAffect
                 +  GazeFollow + Rate +  BehReg +  SocInt +  JointAtt 
                 +  ConvGest +  DistGest + InvCons + SylCons + InvWords + Words 
                 + InvWordCombo + WordCombo + Understanding + InvActions + Pretend 
                 + Blocks + SeqPlay,
                 data=train.bs,
                 method="ctree2",
                 metric="AUC",
                 trControl=fitControl,
                 tuneGrid=ctreegrid)
# get cross-validation results
ctreefit2
# get the final AUC model
ctreefit2$finalModel

plot(ctreefit2$finalModel)
trellis.par.set(caretTheme())
plot(ctreefit2)
# fit the final model to the training data, predict class and node
auc_model <- ctreefit2$finalModel
auc_model_class <- predict(auc_model,data = train.bs)
train.bs$auc_model_class <- auc_model_class
auc_model_node<-predict(auc_model,data = train.bs, type="node")
train.bs$auc_model_node <- auc_model_node

# compute accuracy of the AUC model
aucClassCBE <- predict(auc_model)
tab.auc <- table(aucClassCBE,train.bs$CBE)
sum(diag(tab.auc) / sum(tab.auc))
# run confusion matrix on training data
confusionMatrix(train.bs$auc_model_class, train.bs$CBE)
table(train.bs$CBE,train.bs$auc_model_node)

########## run with Kappa as the outcome
ctreefit3 <- train(CBE~CSBSAge + Sex +  GazeShifts + PosAffect
                 +  GazeFollow + Rate +  BehReg +  SocInt +  JointAtt 
                 +  ConvGest +  DistGest + InvCons + SylCons + InvWords + Words 
                 + InvWordCombo + WordCombo + Understanding + InvActions + Pretend 
                 + Blocks + SeqPlay,
                 data=train.bs,
                 method="ctree2",
                 metric="Kappa",
                 trControl=fitControl,
                 tuneGrid=ctreegrid)
# get cross-validation results
ctreefit3
# get the final AUC model
ctreefit3$finalModel

plot(ctreefit3$finalModel)
trellis.par.set(caretTheme())
plot(ctreefit3)
# fit the final model to the training data, predict class and node
kappa_model <- ctreefit3$finalModel
kappa_model_class <- predict(kappa_model,data = train.bs)
train.bs$kappa_model_class <- kappa_model_class
kappa_model_node<-predict(kappa_model,data = train.bs, type="node")
train.bs$kappa_model_node <- kappa_model_node

# compute accuracy of the kappa model
kappaClassCBE <- predict(kappa_model)
tab.kappa <- table(kappaClassCBE,train.bs$CBE)
sum(diag(tab.kappa) / sum(tab.kappa))
# run confusion matrix on training data
confusionMatrix(train.bs$kappa_model_class,train.bs$CBE)
table(train.bs$CBE,train.bs$kappa_model_node)

########## run with logLoss as the outcome
ctreefit4 <- train(CBE~CSBSAge + Sex +  GazeShifts + PosAffect
                 +  GazeFollow + Rate +  BehReg +  SocInt +  JointAtt 
                 +  ConvGest +  DistGest + InvCons + SylCons + InvWords + Words 
                 + InvWordCombo + WordCombo + Understanding + InvActions + Pretend 
                 + Blocks + SeqPlay,
                 data=train.bs,
                 method="ctree2",
                 metric="logLoss",
                 trControl=fitControl,
                 tuneGrid=ctreegrid)
# get cross-validation results
ctreefit4
# get the final AUC model
ctreefit4$finalModel

plot(ctreefit4$finalModel)
trellis.par.set(caretTheme())
plot(ctreefit4)
# fit the final model to the training data, predict class and node
log_model <- ctreefit4$finalModel
log_model_class < -predict(log_model,data = train.bs)
train.bs$log_model_class <- log_model_class
log_model_node<-predict(log_model,data = train.bs, type="node")
train.bs$log_model_node <- log_model_node

# compute accuracy of the kappa model
logClassCBE <- predict(log_model)
tab.log<-table(logClassCBE, train.bs$CBE)
sum(diag(tab.log) / sum(tab.log))
# run confusion matrix on training data
confusionMatrix(train.bs$log_model_class, train.bs$CBE)
table(train.bs$CBE,train.bs$log_model_node)

########################CV CART RUN WITH BS CLUSTER SCORES####################
# import data: 
cluster_file <- ""
bsclust <- read_excel(cluster_file, guess_max = 5000)
View(bsclust)

# factor group (CBE) and child sex variables
bsclust$Sex <- as.factor(bsclust$Sex)
bsclust$CBE <- as.factor(bsclust$CBE)

# partition the data into a training and test set, stratifying by CBE
## partition size = 80/20
set.seed(16528)
cbeclust.index <- createDataPartition(bsclust$CBE, p = .8, list=FALSE)
train.clust <- bsclust[cbeclust.index,]
test.clust < -bsclust[-cbeclust.index,]

# compare distribution across diagnostic groups for the whole, training, and 
## test set
table(bsclust$CBE)
prop.table(table(bsclust$CBE))
table(train.clust$CBE)
prop.table(table(train.clust$CBE))
table(test.clust$CBE)
prop.table(table(train.clust$CBE))
train.clust<-as.data.frame(train.clust)
str(train.clust)
str(test.clust)
test.clust<-as.data.frame(test.clust)
testdata<-test.bs[,-1]

# create stratified random folds for cross validation analysis using diagnosis
CVindex2 <- createFolds(factor(train.clust$CBE), k = 5, returnTrain = T)

# fit the cross-validation parameters
fitControl2 <- trainControl(index = CVindex2,
                          method = "cv",
                          number=5, 
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary)
# create grid for hyperparameters to tune
ctreegrid <- expand.grid(maxdepth=c(1:5),
                       mincriterion= c(0.95, 0.99))

#train the model using Accuracy as the metric
ctreefitcl1 <- train(CBE~CSBSAge + Sex +  Emotion_EyeGaze + Communication
                   +  Gestures + Sounds +  Words +  Understanding +  Object_Use,
                   data=train.clust,
                   method="ctree2",
                   metric="Accuracy",
                   trControl=fitControl2,
                   tuneGrid=ctreegrid,
                   controls=ctree_control(minbucket = 20))
# review CV results
ctreefitcl1
# review final model
ctreefitcl1$finalModel
# visualization of final model
plot(ctreefitcl1$finalModel )
# plot the final model with fancy graphing colors
plot(ctreefitcl1$finalModel, tp_args = list(fill= c("#8856a7","#f0f0f0","#2ca25f")))

# plot the performance of each hyperparameter
trellis.par.set(caretTheme())
# accuracy
plot(ctreefitcl1)
# auc
plot(ctreefitcl1, metric = "AUC")
# kappa
plot(ctreefitcl1, metric = "Kappa")
# logLoss: For any given problem, a lower log-loss value means better predictions.
## Log Loss is a slight twist on something called the Likelihood Function. 
## In fact, Log Loss is -1 * the log of the likelihood function.
plot(ctreefitcl1, metric = "logLoss")

# extract the best performing model based on the accuracy metric
acc_clmodel <- ctreefitcl1$finalModel
# apply model to the training dataset to predict diagnostic outcome & add to dataframe 
acc_clmodel_class <- predict(acc_clmodel,data = train.clust)
train.clust$acc_clmodel_class <- acc_clmodel_class
# apply model to the training dataset to return the exact node the model predicts & add to dataframe
acc_clmodel_node <- predict(acc_clmodel,data = train.clust, type="node")
train.clust$acc_clmodel_node <- acc_clmodel_node

# predict diagnosis from model and compare with actual data set
accClassCBEcl <- predict(acc_clmodel)
tab.acc.cl <- table(accClassCBEcl, train.clust$CBE)
tab.acc.cl
# compute accuracy of the model
sum(diag(tab.acc.cl) / sum(tab.acc.cl))

# compare with confusion matrix of predictions
confusionMatrix(train.clust$acc_clmodel_class, train.clust$CBE)

# look at breakdown of which node each child
table(train.clust$CBE,train.clust$acc_clmodel_node)

########## run with AUC as the outcome
ctreefitcl2 <- train(CBE~CSBSAge + Sex +  Emotion_EyeGaze + Communication
                   +  Gestures + Sounds +  Words +  Understanding +  Object_Use,
                   data=train.clust,
                   method="ctree2",
                   metric="AUC",
                   trControl=fitControl2,
                   tuneGrid=ctreegrid,
                   controls=ctree_control(minbucket = 20))
# get cross-validation results
ctreefitcl2
# get the final AUC model
ctreefitcl2$finalModel

plot(ctreefitcl2$finalModel)
trellis.par.set(caretTheme())
plot(ctreefitcl2)
# fit the final model to the training data, predict class and node
auc_clmodel <- ctreefitcl2$finalModel
auc_clmodel_class <- predict(auc_clmodel,data = train.clust)
train.clust$auc_clmodel_class <- auc_clmodel_class
auc_clmodel_node <- predict(auc_clmodel,data = train.clust, type="node")
train.clust$auc_clmodel_node <- auc_clmodel_node

# compute accuracy of the AUC model
aucClassCBEcl <- predict(auc_clmodel)
tab.auc.cl <- table(aucClassCBEcl, train.clust$CBE)
sum(diag(tab.auc.cl) / sum(tab.auc.cl))
# run confusion matrix on training data
confusionMatrix(train.clust$auc_clmodel_class, train.clust$CBE)
table(train.clust$CBE,train.clust$auc_clmodel_node)

########## run with Kappa as the outcome
ctreefitcl3 <- train(CBE~CSBSAge + Sex +  Emotion_EyeGaze + Communication
                   +  Gestures + Sounds +  Words +  Understanding +  Object_Use,
                   data=train.clust,
                   method="ctree2",
                   metric="Kappa",
                   trControl=fitControl2,
                   tuneGrid=ctreegrid,
                   controls=ctree_control(minbucket = 20))
# get cross-validation results
ctreefitcl3
# get the final AUC model
ctreefitcl3$finalModel

plot(ctreefitcl3$finalModel)
trellis.par.set(caretTheme())
plot(ctreefitcl3)
# fit the final model to the training data, predict class and node
kappa_clmodel <- ctreefitcl3$finalModel
kappa_clmodel_class <- predict(kappa_clmodel,data = train.clust)
train.clust$kappa_clmodel_class <- kappa_clmodel_class
kappa_clmodel_node <- predict(kappa_clmodel,data = train.clust, type="node")
train.clust$kappa_clmodel_node <- kappa_clmodel_node

# compute accuracy of the kappa model
kappaClassCBEcl <- predict(kappa_clmodel)
tab.kappa.cl <- table(kappaClassCBEcl,train.clust$CBE)
sum(diag(tab.kappa.cl) / sum(tab.kappa.cl))
# run confusion matrix on training data
confusionMatrix(train.clust$kappa_clmodel_class, train.clust$CBE)

table(train.clust$CBE,train.clust$kappa_clmodel_node)

########## run with logLoss as the outcome
ctreefitcl4 <- train(CBE~CSBSAge + Sex +  Emotion_EyeGaze + Communication
                   +  Gestures + Sounds +  Words +  Understanding +  Object_Use,
                   data=train.clust,
                   method="ctree2",
                   metric="logLoss",
                   trControl=fitControl2,
                   tuneGrid=ctreegrid,
                   controls=ctree_control(minbucket = 20))
# get cross-validation results
ctreefitcl4
# get the final AUC model
ctreefitcl4$finalModel

plot(ctreefitcl4$finalModel)
trellis.par.set(caretTheme())
plot(ctreefitcl4)
# fit the final model to the training data, predict class and node
log_clmodel <- ctreefitcl4$finalModel
log_clmodel_class <- predict(log_clmodel,data = train.clust)
train.clust$log_clmodel_class <- log_clmodel_class
log_clmodel_node <- predict(log_clmodel,data = train.clust, type="node")
train.clust$log_clmodel_node <- log_clmodel_node

# compute accuracy of the kappa model
logClassCBEcl <- predict(log_clmodel)
tab.log.cl<-table(logClassCBEcl, train.clust$CBE)
sum(diag(tab.log.cl) / sum(tab.log.cl))
# run confusion matrix on training data
confusionMatrix(train.clust$log_clmodel_class, train.clust$CBE)
table(train.clust$CBE,train.clust$log_clmodel_node)

# predict test data set bases on teh results of the highest performing metric 
acc_clmodel_test <- predict(ctreefitcl1, newdata = test.clust)

confusionMatrix(acc_clmodel_test, test.clust$CBE)

auc_clmodel_test <- predict(ctreefitcl2,newdata = test.clust)
confusionMatrix(auc_clmodel_test, test.clust$CBE)

kappa_clmodel_test <- predict(ctreefitcl3, newdata = test.clust)
confusionMatrix(kappa_clmodel_test,test.clust$CBE)


table(kappa_clmodel_test,test.clust$CBE)
pred.test <- ctreefitcl3 %>%
  predict(test.clust, type="node")
table(train.clust$acc_clmodel_class,train.clust$acc_clmodel_node)

table(train.clust$acc_clmodel_class,train.clust$CBE)


# plot the final model with fancy graphing colors
dev.new()
png("Final2.png", units='in', width=13, height=7,res = 300)
kappaplot <- plot(ctreefitcl3$finalModel, tp_args = list(fill= c("#8856a7","#f0f0f0","#2ca25f")))


dev.off()
plot(ctreefitcl3$finalModel, type="extended", pop=F)
            


kappamodel <- ctree(CBE~CSBSAge + Sex +  Emotion_EyeGaze + Communication
                  +  Gestures + Sounds +  Words +  Understanding +  Object_Use,
                  data=train.clust,
                  testtype="Bonferroni",
                  control = ctree_control(minbucket = 20, maxdepth = 3,mincriterion = 0.99))
st <- as.simpleparty(ctreefitcl3[["finalModel"]])
ctree2()

plot(kappamodel)

###############################################################################
######### load in the rest of the assessment  data 
assess_data <- ""
BSscores<-read_excel(assess_data, guess_max = 5000)

# merge BS composite scores, risk, Mullen, and ADOS with training data set
train.clust.scores<-merge(train.clust,BSscores,by="ChildKey", all.x = T)
# change the predicted node group to a factor for comparisons
train.clust.scores$kappa_clmodel_node<-as.factor(train.clust.scores$kappa_clmodel_node)

# Compare the node groups on BS composites
describeBy(SocialSS+SpeechSS+SymbolicSS+BSTotalSS
           ~kappa_clmodel_node, data = train.clust.scores, mat=F)

# compare the node groups by mullen
describeBy(VisualRecTScore+ FineMotorTScore + RecLangTScore + ExpressLangTScore
           + MSEL_NVT+MSEL_VT+ElcCompositeStandardScore
           ~list(kappa_clmodel_node, CBE), data = train.clust.scores, mat=F)
describeBy(VisualRecTScore+ FineMotorTScore + RecLangTScore + ExpressLangTScore
           + MSEL_NVT+MSEL_VT+ElcCompositeStandardScore
           ~kappa_clmodel_node + CBE, data = train.clust.scores, mat=F)
describeBy(SA_CSS + RRB_CSS + Total_CSS~ 
             kappa_clmodel_node + CBE, data = train.clust.scores, mat=F)

# Proportion of BS risk by node group within column percentages
prop.table(table(train.clust.scores$BSRisk, train.clust.scores$kappa_clmodel_node), 2)
prop.table(table(train.clust.scores$SocialRisk, Train.clust.scores$kappa_clmodel_node))
prop.table(table(train.clust.scores$SpeechRisk, train.clust.scores$kappa_clmodel_node), 2)
prop.table(table(train.clust.scores$SymbolicRisk, train.clust.scores$kappa_clmodel_node),2)
prop.table(table(train.clust.scores$BSTotalRisk, train.clust.scores$kappa_clmodel_node),2)


prop.table(table(train.clust.scores$BSRisk, train.clust.scores$CBE),2)
prop.table(table(train.clust.scores$SocialRisk, train.clust.scores$CBE),2)
prop.table(table(train.clust.scores$SpeechRisk, train.clust.scores$CBE),2)
prop.table(table(train.clust.scores$SymbolicRisk, train.clust.scores$CBE),2)
prop.table(table(train.clust.scores$BSTotalRisk, train.clust.scores$CBE),2)


table(train.clust.scores$BSRisk, train.clust.scores$CBE,train.clust.scores$kappa_clmodel_node)
table(train.clust.scores$SocialRisk,  train.clust.scores$CBE,train.clust.scores$kappa_clmodel_node)
table(train.clust.scores$SpeechRisk, train.clust.scores$CBE,train.clust.scores$kappa_clmodel_node)
table(train.clust.scores$SymbolicRisk, train.clust.scores$CBE,train.clust.scores$kappa_clmodel_node)
table(train.clust.scores$BSTotalRisk, train.clust.scores$CBE,train.clust.scores$kappa_clmodel_node)


describeBy(SocialSS+SpeechSS+SymbolicSS+BSTotalSS
           ~kappa_clmodel_node, data = train.clust.scores, mat=F)
describeBy(VisualRecTScore+ FineMotorTScore + RecLangTScore + ExpressLangTScore
           + MSEL_NVT+MSEL_VT+ElcCompositeStandardScore
           ~ kappa_clmodel_node, data = train.clust.scores, mat=F)
describeBy(SA_CSS + RRB_CSS + Total_CSS~ 
             kappa_clmodel_node , data = train.clust.scores, mat=F)

###############################################################################

# Aim 2: compare children with ASD correctly classfied by the CART model 
## to children incorrectly classified subjects in the ASD group only

# if CBE = ASD and the node group is 4, 5, or 7--this is correctly classified
# if CBE = ASD and the node group is 8, 10, 0r 11--this is wrongly classified
# otherwise, it is not relevant
train.clust.scores$ASDclass <- with(train.clust.scores, 
                                   ifelse(CBE=="ASD" & kappa_clmodel_node=="4"|
                                          CBE=="ASD" & kappa_clmodel_node=="5"|
                                          CBE=="ASD" & kappa_clmodel_node=="7", 
                                          "Correct", 
                                   ifelse(CBE=="ASD" & kappa_clmodel_node=="8"|
                                          CBE=="ASD" & kappa_clmodel_node=="10"|
                                          CBE=="ASD" & kappa_clmodel_node=="11",
                                          "Wrong",NA)))

# simplified version of above conditional using the 
## class prediction variable from the model
train.clust.scores$ASDclassification <- with(train.clust.scores, 
                                             ifelse(CBE=="ASD" & 
                                                      kappa_clmodel_class=="ASD","Correct",
                                                                      ifelse(CBE=="ASD" & 
                                                                               kappa_clmodel_class=="TD","Wrong",NA)))
# double check both approaches
table(train.clust.scores$ASDclass,train.clust.scores$ASDclassification)

# OR JUST USE kappa_clmodel_node variable
describeBy(SocialSS+SpeechSS+SymbolicSS+BSTotalSS
           ~CBE +kappa_clmodel_class, data = train.clust.scores, mat=F)
describeBy(Emotion_EyeGaze+Communication+Gestures
           +Sounds+Words+Understanding+ Object_Use
           ~CBE +kappa_clmodel_class, data = train.clust.scores, mat=F)

describeBy(VisualRecTScore+ FineMotorTScore + RecLangTScore + ExpressLangTScore
           + MSEL_NVT+MSEL_VT+ElcCompositeStandardScore
           ~ CBE +kappa_clmodel_class, data = train.clust.scores, mat=F)
describeBy(SA_CSS + RRB_CSS + Total_CSS~ 
             CBE +kappa_clmodel_class , data = train.clust.scores, mat=F)
# sex by classification
table(train.clust.scores$Sex,train.clust.scores$ASDclassification)
prop.table(table(train.clust.scores$Sex,train.clust.scores$ASDclassification),margin = 2)



# t-test comparisons by classification group for ASD sample--training data
social_t <- t.test(SocialSS~ASDclassification, data=train.clust.scores)
speech_t <- t.test(SpeechSS~ASDclassification, data=train.clust.scores)
symbolic_t <- t.test(SymbolicSS ~ASDclassification, data=train.clust.scores)
bstotal_t <- t.test(BSTotalSS~ASDclassification, data=train.clust.scores)

nvt_t <- t.test(MSEL_NVT~ASDclassification, data=train.clust.scores)
vt_t <- t.test(MSEL_VT~ASDclassification, data=train.clust.scores)
sa_t <- t.test(SA_CSS~ASDclassification, data=train.clust.scores)
rrb_t <- t.test(RRB_CSS~ASDclassification, data=train.clust.scores)

# test sex distribution
sexchi <- chisq.test(train.clust.scores$Sex,train.clust.scores$ASDclassification)
sexchi

# multiple comparison correction
test <- c("social","speech","symbolic","bstotal","nvt","vt","saCSS","rrbCSS","sex")
p_vals <- c(social_t$p.value,speech_t$p.value,symbolic_t$p.value,bstotal_t$p.value,nvt_t$p.value,
          vt_t$p.value,sa_t$p.value,rrb_t$p.value,sexchi$p.value)
ttestpvals <- as.data.frame(cbind(test,p_vals))
ttestpvals$pvalsrnded <- round(p_vals,3)
bonfadj_p_vals <- p.adjust(ttestpvals$p_vals, "bonferroni")
ttestpvals$bonfadj_p_vals <- round(bonfadj_p_vals,3)
ttestpvals

# effect sizes
hedg_g(train.clust.scores, SocialSS~ASDclassification)
hedg_g(train.clust.scores, SpeechSS~ASDclassification)
hedg_g(train.clust.scores, SymbolicSS~ASDclassification)
hedg_g(train.clust.scores, BSTotalSS~ASDclassification)
hedg_g(train.clust.scores, MSEL_NVT~ASDclassification)
hedg_g(train.clust.scores, MSEL_VT~ASDclassification)
hedg_g(train.clust.scores, SA_CSS~ASDclassification)
hedg_g(train.clust.scores, RRB_CSS~ASDclassification)

sexphi <- sqrt(sexchi$statistic/sum(train.clust.scores$CBE=="ASD"))
sexphi

# cluster level t-tests
t.test(Emotion_EyeGaze~ASDclassification, data=train.clust.scores)
t.test(Communication~ASDclassification, data=train.clust.scores)
t.test(Gestures~ASDclassification, data=train.clust.scores)
t.test(Sounds~ASDclassification, data=train.clust.scores)
t.test(Words~ASDclassification, data=train.clust.scores)
t.test(Understanding~ASDclassification, data=train.clust.scores)
t.test(Object_Use~ASDclassification, data=train.clust.scores)


