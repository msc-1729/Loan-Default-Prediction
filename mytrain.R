#importing dataset

train=data.frame(new)

# str(train)
# summary(train)

#excluding uniqueId,date of birth,disbursal date columns from dataframe 

train1<-(train[,c(2:8,10,12:41)])

# sapply(train1,class)
unique(train1[c("EMPLOYMENT_TYPE")])

#removing the  rows containing null values in the dataset

set<-train1[!is.na(train1$EMPLOYMENT_TYPE), ]
set$EMPLOYMENT_TYPE <- as.factor(set$EMPLOYMENT_TYPE)

unique(set[c("EMPLOYMENT_TYPE")])

set$EMPLOYMENT_TYPE <- as.numeric(set$EMPLOYMENT_TYPE)

unique(set[c("PERFORM_CNS_SCORE_DESCRIPTION")])
set<-set[!is.na(set$PERFORM_CNS_SCORE_DESCRIPTION), ]
set$PERFORM_CNS_SCORE_DESCRIPTION <- as.factor(set$PERFORM_CNS_SCORE_DESCRIPTION)

unique(set[c("PERFORM_CNS_SCORE_DESCRIPTION")])

set$PERFORM_CNS_SCORE_DESCRIPTION <- as.numeric(set$PERFORM_CNS_SCORE_DESCRIPTION)


library(caret)
#normalising the numeric data
set1 <- as.data.frame(apply(set[, c(1,2,17,22:24,28:32)], 2, function(x) (x - min(x))/(max(x)-min(x))))
str(set1)
set1$LTV <- set$LTV
set1$BRANCH_ID <- set$BRANCH_ID
set1$SUPPLIER_ID <- set$SUPPLIER_ID
set1$MANUFACTURER_ID <- set$MANUFACTURER_ID
set1$CURRENT_PINCODE_ID <- set$CURRENT_PINCODE_ID
set1$EMPLOYMENT_TYPE <- set$EMPLOYMENT_TYPE
set1$STATE_ID <- set$STATE_ID
set1$EMPLOYEE_CODE_ID <- set$EMPLOYEE_CODE_ID
set1$MOBILENO_AVL_FLAG  <- set$MOBILENO_AVL_FLAG
set1$AADHAR_FLAG <- set$AADHAR_FLAG
set1$PAN_FLAG <- set$PAN_FLAG
set1$VOTERID_FLAG <- set$VOTERID_FLAG
set1$DRIVING_FLAG <- set$DRIVING_FLAG
set1$PASSPORT_FLAG  <- set$PASSPORT_FLAG
set1$PERFORM_CNS_SCORE_DESCRIPTION <- set$PERFORM_CNS_SCORE_DESCRIPTION
set1$PRI_NO_OF_ACCTS   <- set$PRI_NO_OF_ACCTS
set1$PRI_ACTIVE_ACCTS <- set$PRI_ACTIVE_ACCTS
set1$PRI_OVERDUE_ACCTS  <- set$PRI_OVERDUE_ACCTS
set1$SEC_NO_OF_ACCTS   <- set$SEC_NO_OF_ACCTS
set1$SEC_ACTIVE_ACCTS <- set$SEC_ACTIVE_ACCTS
set1$SEC_OVERDUE_ACCTS<- set$SEC_OVERDUE_ACCTS
set1$NEW_ACCTS_IN_LAST_SIX_MONTHS <- set$NEW_ACCTS_IN_LAST_SIX_MONTHS
set1DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS <- set$DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS
set1$AVERAGE_ACCT_AGE  <- set$AVERAGE_ACCT_AGE
set1$CREDIT_HISTORY_LENGTH  <- set$CREDIT_HISTORY_LENGTH
set1$NO_OF_INQUIRIES <- set$NO_OF_INQUIRIES
set1$LOAN_DEFAULT <- set$LOAN_DEFAULT


part0<-subset(set1,  set1$LOAN_DEFAULT==0)
part1<-subset(set1,  set1$LOAN_DEFAULT==1)

part2<-part0[1:10000,]
part3<-part1[1:10000,]
fpart<-rbind(part2,part3)

#feature selection first method
# nrow(fpart)
library(Boruta)
library(mlbench)
library(randomForest)
# ncol(fpart)
# set.seed(111)
# select<-Boruta(LOAN_DEFAULT ~ . , data = fpart , doTrace =2 , maxRuns =100)
# print(select)
# plot(select,las=2,cen.axis=0.9)
# plotImpHistory(select)
# attStats(select)

#feature selection second method
library(caret)
set.seed(100)
rPartMod <- train(LOAN_DEFAULT ~ ., data=set1, method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)



part4<-part0[1:40000,]
part5<-part1[1:40000,]
fpart1<-rbind(part4,part5)


#random forest

# getNonRejectedFormula(select)

fpart1$LOAN_DEFAULT=as.factor(fpart1$LOAN_DEFAULT)
set.seed(222)

#data splitting
ind<-sample(2,nrow(fpart1),replace=T,prob=c(0.7,0.3))
trainfin<-fpart1[ind==1,]
test<-fpart1[ind==2,]
########################################################
#all variables
set.seed(333)
 rf60<-randomForest(LOAN_DEFAULT~., data=trainfin)
 rf60
 p<-predict(rf60,test,type="claSS")
 print(p)
confusionMatrix(p,test$LOAN_DEFAULT)

require(pROC)
point<-predict(rf60,test,type="prob")
ROC_rfad <- roc(test$LOAN_DEFAULT, point[,2])
ROC_rf_aucrt <- auc(ROC_rfad)
plot(ROC_rfad, col = "green", main = "ROC For Random Forest (GREEN)")
paste("Accuracy % of random forest: ", mean(test$LOAN_DEFAULT == round(point[,2], digits = 0)))
paste("Area under curve of random forest: ", ROC_rf_aucrt)
library(MLmetrics)
F1_Score(y_true=test$LOAN_DEFAULT,y_pred=p)
precision(test$LOAN_DEFAULT,p)
recall(test$LOAN_DEFAULT,p)

###########################################################
#27 variables
rf27<-randomForest(LOAN_DEFAULT~ DISBURSED_AMOUNT + ASSET_COST + PERFORM_CNS_SCORE + 
                     PRI_CURRENT_BALANCE + PRI_SANCTIONED_AMOUNT + PRI_DISBURSED_AMOUNT + 
                     PRIMARY_INSTAL_AMT + LTV + BRANCH_ID + SUPPLIER_ID + MANUFACTURER_ID + 
                     CURRENT_PINCODE_ID + EMPLOYMENT_TYPE + STATE_ID + EMPLOYEE_CODE_ID + 
                     AADHAR_FLAG + PAN_FLAG + VOTERID_FLAG + DRIVING_FLAG + PERFORM_CNS_SCORE_DESCRIPTION + 
                     PRI_NO_OF_ACCTS + PRI_ACTIVE_ACCTS + PRI_OVERDUE_ACCTS + 
                     NEW_ACCTS_IN_LAST_SIX_MONTHS + AVERAGE_ACCT_AGE + CREDIT_HISTORY_LENGTH + 
                     NO_OF_INQUIRIES , data=trainfin)
rf27

library(e1071)

#testing
p1<-predict(rf27,test,type="class")
confusionMatrix(p1,test$LOAN_DEFAULT)

require(pROC)
p3<-predict(rf27,test,type="prob")
ROC_rf <- roc(test$LOAN_DEFAULT, p3[,2])
ROC_rf_auc <- auc(ROC_rf)
plot(ROC_rf, col = "green", main = "ROC For Random Forest (GREEN)")
paste("Accuracy % of random forest: ", mean(test$LOAN_DEFAULT == round(p3[,2], digits = 0)))
paste("Area under curve of random forest: ", ROC_rf_auc)
library(MLmetrics)
F1_Score(y_true=test$LOAN_DEFAULT,y_pred=p1)
precision(test$LOAN_DEFAULT,p1)
recall(test$LOAN_DEFAULT,p1)

#########################################################
##20 variables
rf20<-randomForest(LOAN_DEFAULT~PERFORM_CNS_SCORE     +
                     PERFORM_CNS_SCORE_DESCRIPTION   +
                     LTV                             +
                     DISBURSED_AMOUNT                +
                     PRI_SANCTIONED_AMOUNT           +
                     PRI_DISBURSED_AMOUNT            +
                     PRI_CURRENT_BALANCE             +
                     STATE_ID                        +
                     EMPLOYMENT_TYPE                 +
                     PAN_FLAG                       +
                     ASSET_COST                     +
                     DRIVING_FLAG                   +
                     CREDIT_HISTORY_LENGTH          +
                     CURRENT_PINCODE_ID             +
                     VOTERID_FLAG                   +
                     NO_OF_INQUIRIES                +
                     SEC_OVERDUE_ACCTS              +
                     AADHAR_FLAG                    +
                     PRI_ACTIVE_ACCTS               +
                     SEC_INSTAL_AMT              , data=trainfin)
rf20

#testing
p2<-predict(rf20,test,type="class")
confusionMatrix(p2,test$LOAN_DEFAULT)

library(ROCR)

#metrics
require(pROC)
point2<-predict(rf20,test,type="prob")
ROC_rfas <- roc(test$LOAN_DEFAULT, point2[,2])
ROC_rf_aucas <- auc(ROC_rfas)
plot(ROC_rfas, col = "green", main = "ROC For Random Forest (GREEN)")
paste("Accuracy % of random forest: ", mean(test$LOAN_DEFAULT == round(point2[,2], digits = 0)))
paste("Area under curve of random forest: ", ROC_rf_aucas)
library(MLmetrics)
F1_Score(y_true=test$LOAN_DEFAULT,y_pred=p2)
precision(test$LOAN_DEFAULT,p2)
recall(test$LOAN_DEFAULT,p2)




##########################################################################


#logistic regression

########################################################################
##27 variables

log<-glm(LOAN_DEFAULT~DISBURSED_AMOUNT + ASSET_COST + PERFORM_CNS_SCORE + 
           PRI_CURRENT_BALANCE + PRI_SANCTIONED_AMOUNT + PRI_DISBURSED_AMOUNT + 
           PRIMARY_INSTAL_AMT + LTV + BRANCH_ID + SUPPLIER_ID + MANUFACTURER_ID + 
           CURRENT_PINCODE_ID + EMPLOYMENT_TYPE + STATE_ID + EMPLOYEE_CODE_ID + 
           AADHAR_FLAG + PAN_FLAG + VOTERID_FLAG + DRIVING_FLAG + PERFORM_CNS_SCORE_DESCRIPTION + 
           PRI_NO_OF_ACCTS + PRI_ACTIVE_ACCTS + PRI_OVERDUE_ACCTS + 
           NEW_ACCTS_IN_LAST_SIX_MONTHS + AVERAGE_ACCT_AGE + CREDIT_HISTORY_LENGTH + 
           NO_OF_INQUIRIES,family=binomial(link="logit"),data=trainfin)
summary(log)
anova(log,test="Chisq")
library(pscl)

#testing
newdata1 <- predict(log, newdata =test, type = "response")
newdata1<-ifelse(newdata1>0.5,1,0)
misclass<-mean(newdata1!=test$LOAN_DEFAULT)
print(paste('accuracy',1-misclass
            ))
#metrics

confusionMatrix(as.factor(newdata1),test$LOAN_DEFAULT)

require(pROC)
library(ROCR)
ROCRpred <- prediction(newdata1, test$LOAN_DEFAULT)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
roc_lr<-roc(test$LOAN_DEFAULT,newdata1)
roc_lr_auc<-auc(roc_lr)
plot(roc_lr, col = "green", main = "ROC For logistic regression (GREEN)")
paste("Accuracy % of logistic regression: ", mean(test$LOAN_DEFAULT == round(newdata1, digits = 0)))
paste("Area under curve of random forest: ", roc_lr_auc)

library(MLmetrics)
F1_Score(y_true=test$LOAN_DEFAULT,y_pred=as.factor(newdata1))
precision(test$LOAN_DEFAULT,as.factor(newdata1))
recall(test$LOAN_DEFAULT,as.factor(newdata1))


#############################################################################
#logistic regression
#all variables
log1<-glm(LOAN_DEFAULT~.,family=binomial(link="logit"),data=trainfin)
summary(log1)
anova(log1,test="Chisq")
library(pscl)

#testing
newdata12 <- predict(log1, newdata =test, type = "response")
newdata12<-ifelse(newdata12>0.5,1,0)
misclass1<-mean(newdata12!=test$LOAN_DEFAULT)
print(paste('accuracy',1-misclass1
))
#metrics

confusionMatrix(as.factor(newdata12),test$LOAN_DEFAULT)

require(pROC)
library(ROCR)
ROCRpred1 <- prediction(newdata12, test$LOAN_DEFAULT)
ROCRperf1 <- performance(ROCRpred1, 'tpr','fpr')
plot(ROCRperf1, colorize = TRUE, text.adj = c(-0.2,1.7))
roc_lr1<-roc(test$LOAN_DEFAULT,newdata12)
roc_lr_auc1<-auc(roc_lr1)
plot(roc_lr1, col = "green", main = "ROC For logistic regression (GREEN)")
paste("Accuracy % of logistic regression: ", mean(test$LOAN_DEFAULT == round(newdata12, digits = 0)))
paste("Area under curve of random forest: ", roc_lr_auc1)

library(MLmetrics)
F1_Score(y_true=test$LOAN_DEFAULT,y_pred=as.factor(newdata12))
precision(test$LOAN_DEFAULT,as.factor(newdata12))
recall(test$LOAN_DEFAULT,as.factor(newdata12))

####################################################################################################
#logistic regression 
##20 variables
log2<-glm(LOAN_DEFAULT~PERFORM_CNS_SCORE     +
            PERFORM_CNS_SCORE_DESCRIPTION   +
            LTV                             +
            DISBURSED_AMOUNT                +
            PRI_SANCTIONED_AMOUNT           +
            PRI_DISBURSED_AMOUNT            +
            PRI_CURRENT_BALANCE             +
            STATE_ID                        +
            EMPLOYMENT_TYPE                 +
            PAN_FLAG                       +
            ASSET_COST                     +
            DRIVING_FLAG                   +
            CREDIT_HISTORY_LENGTH          +
            CURRENT_PINCODE_ID             +
            VOTERID_FLAG                   +
            NO_OF_INQUIRIES                +
            SEC_OVERDUE_ACCTS              +
            AADHAR_FLAG                    +
            PRI_ACTIVE_ACCTS               +
            SEC_INSTAL_AMT              ,family=binomial(link="logit"),data=trainfin)
summary(log2)
anova(log2,test="Chisq")
library(pscl)

#testing
newdata13 <- predict(log2, newdata =test, type = "response")
newdata13<-ifelse(newdata13>0.5,1,0)
misclass3<-mean(newdata13!=test$LOAN_DEFAULT)
print(paste('accuracy',1-misclass3
))
#metrics

confusionMatrix(as.factor(newdata13),test$LOAN_DEFAULT)

require(pROC)
library(ROCR)
ROCRpred2 <- prediction(newdata13, test$LOAN_DEFAULT)
ROCRperf2 <- performance(ROCRpred2, 'tpr','fpr')
plot(ROCRperf2, colorize = TRUE, text.adj = c(-0.2,1.7))
roc_lr2<-roc(test$LOAN_DEFAULT,newdata13)
roc_lr_auc2<-auc(roc_lr2)
plot(roc_lr2, col = "green", main = "ROC For logistic regression (GREEN)")
paste("Accuracy % of logistic regression: ", mean(test$LOAN_DEFAULT == round(newdata13, digits = 0)))
paste("Area under curve of random forest: ", roc_lr_auc2)

library(MLmetrics)
F1_Score(y_true=test$LOAN_DEFAULT,y_pred=as.factor(newdata13))
precision(test$LOAN_DEFAULT,as.factor(newdata13))
recall(test$LOAN_DEFAULT,as.factor(newdata13))


################################################################################
#decision tree
########################################################################
##27 variables
library(rpart.plot)
fit <- rpart(LOAN_DEFAULT~ DISBURSED_AMOUNT + ASSET_COST + PERFORM_CNS_SCORE + 
               PRI_CURRENT_BALANCE + PRI_SANCTIONED_AMOUNT + PRI_DISBURSED_AMOUNT + 
               PRIMARY_INSTAL_AMT + LTV + BRANCH_ID + SUPPLIER_ID + MANUFACTURER_ID + 
               CURRENT_PINCODE_ID + EMPLOYMENT_TYPE + STATE_ID + EMPLOYEE_CODE_ID + 
               AADHAR_FLAG + PAN_FLAG + VOTERID_FLAG + DRIVING_FLAG + PERFORM_CNS_SCORE_DESCRIPTION + 
               PRI_NO_OF_ACCTS + PRI_ACTIVE_ACCTS + PRI_OVERDUE_ACCTS + 
               NEW_ACCTS_IN_LAST_SIX_MONTHS + AVERAGE_ACCT_AGE + CREDIT_HISTORY_LENGTH + 
               NO_OF_INQUIRIES, data =trainfin, method = 'class',minsplit=2)
rpart.plot(fit, extra = 106, type=5)

predict_unseen <-predict(fit, test, type = 'class')
table_mat <- table(test$LOAN_DEFAULT, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

#metrics 

library(ROCR)
DTPrediction <- predict(fit, test,type = "prob")
Prediction <- prediction(DTPrediction[,2],test$LOAN_DEFAULT)
performances <- performance(Prediction, "tpr","fpr")
# plotting ROC curve
plot(performances,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
aucDT <- performance(Prediction, measure = "auc")
aucDT <- aucDT@y.values[[1]]
aucDT

library(MLmetrics)
F1_Score(y_true=test$LOAN_DEFAULT,y_pred=predict_unseen)
precision(test$LOAN_DEFAULT,predict_unseen)
recall(test$LOAN_DEFAULT,predict_unseen)

##############################################################################
##all variables
library(rpart.plot)
fit1 <- rpart(LOAN_DEFAULT~ ., data =trainfin, method = 'class')
rpart.plot(fit1, extra = 106)

predict_unseen1 <-predict(fit1, test, type = 'class')
table_mat1 <- table(test$LOAN_DEFAULT, predict_unseen1)
table_mat1
accuracy_Test1 <- sum(diag(table_mat1)) / sum(table_mat1)
print(paste('Accuracy for test', accuracy_Test1))

#metrics 

library(ROCR)
DTPrediction1 <- predict(fit1, test,type = "prob")
Prediction1 <- prediction(DTPrediction[,2],test$LOAN_DEFAULT)
performances1 <- performance(Prediction1, "tpr","fpr")
# plotting ROC curve
plot(performances1,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
aucDT1 <- performance(Prediction1, measure = "auc")
aucDT1 <- aucDT1@y.values[[1]]
aucDT1

library(MLmetrics)
F1_Score(y_true=test$LOAN_DEFAULT,y_pred=predict_unseen1)
precision(test$LOAN_DEFAULT,predict_unseen1)
recall(test$LOAN_DEFAULT,predict_unseen1)
##################################################################################
##20 variables
library(rpart.plot)
fit2 <- rpart(LOAN_DEFAULT~PERFORM_CNS_SCORE     +
                PERFORM_CNS_SCORE_DESCRIPTION   +
                LTV                             +
                DISBURSED_AMOUNT                +
                PRI_SANCTIONED_AMOUNT           +
                PRI_DISBURSED_AMOUNT            +
                PRI_CURRENT_BALANCE             +
                STATE_ID                        +
                EMPLOYMENT_TYPE                 +
                PAN_FLAG                       +
                ASSET_COST                     +
                DRIVING_FLAG                   +
                CREDIT_HISTORY_LENGTH          +
                CURRENT_PINCODE_ID             +
                VOTERID_FLAG                   +
                NO_OF_INQUIRIES                +
                SEC_OVERDUE_ACCTS              +
                AADHAR_FLAG                    +
                PRI_ACTIVE_ACCTS               +
                SEC_INSTAL_AMT              , data =trainfin, method = 'class')
rpart.plot(fit2, extra = 106)

predict_unseen2 <-predict(fit2, test, type = 'class')
table_mat2 <- table(test$LOAN_DEFAULT, predict_unseen2)
table_mat2
accuracy_Test2 <- sum(diag(table_mat2)) / sum(table_mat2)
print(paste('Accuracy for test', accuracy_Test2))

#metrics 

library(ROCR)
DTPrediction2 <- predict(fit2, test,type = "prob")
Prediction2 <- prediction(DTPrediction2[,2],test$LOAN_DEFAULT)
performances2 <- performance(Prediction2, "tpr","fpr")
# plotting ROC curve
plot(performances2,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
aucDT2 <- performance(Prediction2, measure = "auc")
aucDT2 <- aucDT2@y.values[[1]]
aucDT2

library(MLmetrics)
F1_Score(y_true=test$LOAN_DEFAULT,y_pred=predict_unseen2)
precision(test$LOAN_DEFAULT,predict_unseen2)
recall(test$LOAN_DEFAULT,predict_unseen2)
###################################################################################

#naivebayes
############################################################################
#27 variables
library(e1071)
set.seed(120)  # Setting Seed 
classifier_cl <- naiveBayes(LOAN_DEFAULT~DISBURSED_AMOUNT + ASSET_COST + PERFORM_CNS_SCORE + 
                              PRI_CURRENT_BALANCE + PRI_SANCTIONED_AMOUNT + PRI_DISBURSED_AMOUNT + 
                              PRIMARY_INSTAL_AMT + LTV + BRANCH_ID + SUPPLIER_ID + MANUFACTURER_ID + 
                              CURRENT_PINCODE_ID + EMPLOYMENT_TYPE + STATE_ID + EMPLOYEE_CODE_ID + 
                              AADHAR_FLAG + PAN_FLAG + VOTERID_FLAG + DRIVING_FLAG + PERFORM_CNS_SCORE_DESCRIPTION + 
                              PRI_NO_OF_ACCTS + PRI_ACTIVE_ACCTS + PRI_OVERDUE_ACCTS + 
                              NEW_ACCTS_IN_LAST_SIX_MONTHS + AVERAGE_ACCT_AGE + CREDIT_HISTORY_LENGTH + 
                              NO_OF_INQUIRIES, data = trainfin) 
classifier_cl 

# Predicting on test data' 
y_pred <- predict(classifier_cl, newdata = test) 

# Confusion Matrix 
cm <- table(test$LOAN_DEFAULT, y_pred) 
cm 

# Model Evauation 
confusionMatrix(cm) 

library(MLmetrics)
F1_Score(y_true=test$LOAN_DEFAULT,y_pred)
precision(test$LOAN_DEFAULT,y_pred)
recall(test$LOAN_DEFAULT,y_pred)

library(ROCR)
DTPrediction1 <- predict(classifier_cl , test,type = "raw")
Prediction1 <- prediction(DTPrediction1[,2],test$LOAN_DEFAULT)
performances1 <- performance(Prediction1, "tpr","fpr")
# plotting ROC curve
plot(performances,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
aucDT3 <- performance(Prediction1, measure = "auc")
aucDT3 <- aucDT3@y.values[[1]]
aucDT3
#####################################################################
#all variables
library(e1071)
set.seed(120)  # Setting Seed 
classifier_cl1 <- naiveBayes(LOAN_DEFAULT~., data = trainfin) 
classifier_cl1 

# Predicting on test data' 
y_pred1 <- predict(classifier_cl1, newdata = test) 

# Confusion Matrix 
cm1 <- table(test$LOAN_DEFAULT, y_pred1) 
cm1

# Model Evauation 
confusionMatrix(cm1) 

library(MLmetrics)
F1_Score(y_true=test$LOAN_DEFAULT,y_pred1)
precision(test$LOAN_DEFAULT,y_pred1)
recall(test$LOAN_DEFAULT,y_pred1)

library(ROCR)
DTPrediction123 <- predict(classifier_cl1 , test,type = "raw")
Prediction123 <- prediction(DTPrediction123[,2],test$LOAN_DEFAULT)
performances123 <- performance(Prediction123, "tpr","fpr")
# plotting ROC curve
plot(performances123,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
aucDT123 <- performance(Prediction123, measure = "auc")
aucDT123 <- aucDT123@y.values[[1]]
aucDT123
#######################################################################
##with 20 variables
library(e1071)
set.seed(120)  # Setting Seed 
classifier_cl4 <- naiveBayes(LOAN_DEFAULT~PERFORM_CNS_SCORE     +
                             PERFORM_CNS_SCORE_DESCRIPTION   +
                             LTV                             +
                             DISBURSED_AMOUNT                +
                             PRI_SANCTIONED_AMOUNT           +
                             PRI_DISBURSED_AMOUNT            +
                             PRI_CURRENT_BALANCE             +
                             STATE_ID                        +
                             EMPLOYMENT_TYPE                 +
                             PAN_FLAG                       +
                             ASSET_COST                     +
                             DRIVING_FLAG                   +
                             CREDIT_HISTORY_LENGTH          +
                             CURRENT_PINCODE_ID             +
                             VOTERID_FLAG                   +
                             NO_OF_INQUIRIES                +
                             SEC_OVERDUE_ACCTS              +
                             AADHAR_FLAG                    +
                             PRI_ACTIVE_ACCTS               +
                             SEC_INSTAL_AMT                 , data = trainfin) 
classifier_cl4 

# Predicting on test data' 
y_pred4 <- predict(classifier_cl, newdata = test) 

# Confusion Matrix 
cm4 <- table(test$LOAN_DEFAULT, y_pred4) 
cm4

# Model Evauation 
confusionMatrix(cm4) 

library(MLmetrics)
F1_Score(y_true=test$LOAN_DEFAULT,y_pred4)
precision(test$LOAN_DEFAULT,y_pred4)
recall(test$LOAN_DEFAULT,y_pred4)

library(ROCR)
DTPrediction198 <- predict(classifier_cl4 , test,type = "raw")
Prediction198 <- prediction(DTPrediction198[,2],test$LOAN_DEFAULT)
performances198 <- performance(Prediction198, "tpr","fpr")
# plotting ROC curve
plot(performances198,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
aucDT198 <- performance(Prediction198, measure = "auc")
aucDT198 <- aucDT198@y.values[[1]]
aucDT198

