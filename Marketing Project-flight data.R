setwd("--------------------------")
flight_data <- read.csv("Marketing Project-Flight data.csv", header = TRUE, na.strings=c("","NA"))
Survey_data <- read.csv("Marketing Project-Survey data.csv", header = TRUE, na.strings=c("","NA"))
Dataset_flightsurvey <- merge(flight_data, Survey_data, by.x = "ID", by.y = "Id")
write.csv(Dataset_flightsurvey, "Dataset_flightsurvey.csv")
View(Dataset_flightsurvey)
summary(Dataset_flightsurvey)
names(Dataset_flightsurvey)
str(Dataset_flightsurvey)
head(Dataset_flightsurvey)
colSums(is.na(Dataset_flightsurvey))


library(DataExplorer)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(scales)
library(dplyr)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(dlookr)
library(SmartEDA)

seed = 123
set.seed(seed)
correlation<- plot_correlation(Dataset_flightsurvey[,-1])
ggsave("correlation.jpeg", correlation, width = 36, height = 36, units = "cm")
flightdata_correlation<- plot_correlation(flight_data[,-1])
ggsave("flightdata_correlation.jpeg", flightdata_correlation, width = 25, height = 25, units = "cm")


plotUNIVFunc <- function(x, na.rm = TRUE) {
  nm <- names(x)
  for (i in seq_along(nm)) {
    plots <-ggplot(x,aes_string(x = nm[i]), fill = factor(i), 
                   levels = c("extremely poor","poor", "need improvement" , "acceptable", "good", "excellent")) + 
      geom_bar(alpha = .5,fill = "dodgerblue") +
      geom_text(stat = 'count', aes(label=scales::percent(..count../sum(..count.., vjust = -0.2)))) +
      geom_text(stat = 'count', aes(label=..count.., vjust = -1))
    ggsave(plots,filename=paste("EDA\\Univvarimage\\myplot",nm[i],".png",sep=""), width = 15, height = 15, units = "cm")
  }
}

plotUNIVFunc(Dataset_flightsurvey[,-1]) ## execute function

plot_bivarFunc <- function(x, na.rm = TRUE) {
  nm <- names(x)
  for (i in seq_along(nm)) {
    for (j in seq_along(nm)) {
      
      plots <-ggplot(x,aes_string(x = nm[j], fill = nm[i])) + 
        geom_bar(position = "fill") + 
        geom_text(aes(label=scales::percent(..count../sum(..count..))),
                  stat='count',position=position_fill(vjust=0.5))  #, label = paste0(percentage,"%"),size=4)
      ggsave(plots,filename=paste("EDA\\bivarimage\\myplot bivariate",nm[i],nm[j],".png",sep=" "))
    }
    
  }
}

plot_bivarFunc(Dataset_flightsurvey[,-1]) ## execute function

basic_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}

eda <- basic_eda(Dataset_flightsurvey[,-1])
freq(Dataset_flightsurvey, path_out = "EDA/freq")
ExpReport(Dataset_flightsurvey, op_file = "smarteda.html")

diagnose_report(Dataset_flightsurvey[,-1], output_format = "html")
normality(Dataset_flightsurvey[,-1])
eda_report(Dataset_flightsurvey, target = "Satisfaction", output_format = "html", output_file = "EDA_report.html")
plot_missing(Dataset_flightsurvey[,-1])
total_count <- dim(Dataset_flightsurvey)
percent_missing <- (sum(is.na(Dataset_flightsurvey))/ (total_count[1]* total_count[2]))*100
print(percent_missing)  


###------------milestone 2-----------------------------------------------###
##--------------Removal of unwanted variables----------------------------------
#ID is unique variable so we will remove ID from the dataset for further analysis. 

Dataset_flightsurvey <- Dataset_flightsurvey[,-1]

###--------------Missing Value Treatment ------------------------

library(missForest)
set.seed(seed)

missing_value_names <- colnames(Dataset_flightsurvey)[colSums(is.na(Dataset_flightsurvey)) > 0] 
dataset_mf <- missForest(Dataset_flightsurvey[ ,c(2,4,8,11,12,18)])

dataset_mf_new <- data.frame(dataset_mf$ximp)
write.csv(dataset_mf_new, "missing_treatment.csv")
dataset_rm_na <- Dataset_flightsurvey[, -which(names(Dataset_flightsurvey) %in% missing_value_names)]
colnames(dataset_mf_new)[colSums(is.na(dataset_mf_new))>0]
summary(dataset_mf_new)


plotUNIVmissing <- function(x, na.rm = TRUE) {
  nm <- names(x)
  for (i in seq_along(nm)) {
    plots <-ggplot(x,aes_string(x = nm[i]), fill = factor(i)) + 
      geom_bar(alpha = .5,fill = "dodgerblue") +
      geom_text(stat = 'count', aes(label=scales::percent(..count../sum(..count.., vjust = -0.2)))) +
      geom_text(stat = 'count', aes(label=..count.., vjust = -1))
    ggsave(plots,filename=paste("EDA\\missingdata\\myplot",nm[i],".png",sep=""), width = 15, height = 15, units = "cm")
  }
}

plotUNIVmissing(dataset_mf_new)
dataset_mf_new <- cbind(dataset_rm_na, dataset_mf_new)
#write.csv(dataset_mf_new, "dataset_mf_new.csv")
#summary(dataset_mf_new)

###---------Chi square test-------------------------------------------------------------------

chiSquare(dataset_mf_new$Satisfaction~., dataset_mf_new)


#----------------------------CAP the outlier----------------------------------------------------

#dataset_mf_new <- read.csv("dataset_mf_new.csv", header = TRUE)
#dataset_mf_new <- dataset_mf_new[, 2:25]
str(dataset_mf_new)
boxplot(dataset_mf_new[,-c(1,3, 6:19, 21:23)])
set.seed(1234)

capoutlier <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

dataset_mf_new$Flight_Distance <- capoutlier(dataset_mf_new$Flight_Distance)
dataset_mf_new$DepartureDelayin_Mins <- capoutlier(dataset_mf_new$DepartureDelayin_Mins)
dataset_mf_new$ArrivalDelayin_Mins <- capoutlier(dataset_mf_new$ArrivalDelayin_Mins)

jpeg("EDA\\Flightdistance_outlier.jpeg", width = 600, height = 600)
plotpar <- par(mfrow = c(2, 2),
               pty = "m") # 2 x 2 pictures on one plot
boxplot(Dataset_flightsurvey$Flight_Distance, main = "With outlier",col = "lightblue" )
hist(Dataset_flightsurvey$Flight_Distance, main = "With outlier", xlab = "Flight Distance", col = "lightblue")
boxplot(dataset_mf_new$Flight_Distance, main = "Without outlier", col = "pink")
hist(dataset_mf_new$Flight_Distance, main = "Without outlier", xlab = "Flight Distance",col = "pink")
mtext("Flight distance - Outlier treatment", side = 3, line = -1, outer = TRUE, cex = 1.2)
par(plotpar)
dev.off()


jpeg("EDA\\DepartureDelayin_Mins_outlier.jpeg", width = 600, height = 600)
plotpar <- par(mfrow = c(2, 2),
               pty = "m") # 2 x 2 pictures on one plot
boxplot(Dataset_flightsurvey$DepartureDelayin_Mins, main = "With outlier",col = "lightblue" )
hist(Dataset_flightsurvey$DepartureDelayin_Mins, main = "With outlier", xlab = "Departure Delay in Mins", col = "lightblue")
boxplot(dataset_mf_new$DepartureDelayin_Mins, main = "Without outlier", col = "pink")
hist(dataset_mf_new$DepartureDelayin_Mins, main = "Without outlier", xlab = "Departure Delay in Mins",col = "pink")
mtext("Departure Delay in Mins - Outlier treatment", side = 3, line = -1, outer = TRUE, cex = 1.2)
par(plotpar)
dev.off()

jpeg("EDA\\Arrival Delay in Mins_outlier.jpeg", width = 600, height = 600)
plotpar <- par(mfrow = c(2, 2),
               pty = "m") # 2 x 2 pictures on one plot
boxplot(Dataset_flightsurvey$ArrivalDelayin_Mins, main = "With outlier",col = "lightblue" )
hist(Dataset_flightsurvey$ArrivalDelayin_Mins, main = "With outlier", xlab = "Arrival Delay in Mins", col = "lightblue")
boxplot(dataset_mf_new$ArrivalDelayin_Mins, main = "Without outlier", col = "pink")
hist(dataset_mf_new$ArrivalDelayin_Mins, main = "Without outlier", xlab = "Arrival Delay in Mins",col = "pink")
mtext("Arrival Delay in Mins - Outlier treatment", side = 3, line = -1, outer = TRUE, cex = 1.2)
par(plotpar)
dev.off()


#________________Feature selection _____________________________________##

library(Boruta)
boruta.train <- Boruta(dataset_mf_new$Satisfaction~.,data = dataset_mf_new, doTrace = 0)
print(boruta.train)

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 3,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.6)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
getSelectedAttributes(final.boruta, withTentative = F)
boruta_df <- attStats(final.boruta)
write.csv(boruta_df, "boruta_df.csv")

print(boruta_df)



##### ------------------- find out influencing variable-----------------------------


library(tidyverse)
library(caret)

library(nnet)
#install.packages("e1071")

library(e1071)
library(car)

##_____ logistic regression______________________________________________________________________
mlr <- glm(Satisfaction~., dataset_mf_new, family = binomial)
summary(mlr)
prob_mlr_train <- predict(mlr, training_set_full)
prob_mlr_train_factor <- factor(ifelse(prob_mlr_train > 0.5,"satisfied", "neutral or dissatisfied"))
confusionMatrix(prob_mlr_train_factor , training_set_full$Satisfaction)
prob_mlr <- predict(mlr, test_set_full)
prob_mlr_factor <- factor(ifelse(prob_mlr > 0.5,"satisfied", "neutral or dissatisfied"))
confusionMatrix(prob_mlr_factor, test_set_full$Satisfaction)
summary(mlr)$coef
anova(mlr, test="Chisq")


#install.packages("randomForest")
library(randomForest)
library(caret)
rf_model_full <- randomForest(Satisfaction~.,dataset_mf_new)
rf_model_full
varImp(rf_model_full)
varImpPlot(rf_model_full, main = "Influencing Variables towards 'Satisfied'")

tRndFor_full = tuneRF(x = training_set_full[ ,-6],
                      y=training_set_full$Satisfaction,
                      mtryStart = 3,
                      ntreeTry = 51,
                      stepFactor = 1.5,
                      improve = 0.0001,
                      trace=TRUE,
                      plot = TRUE,
                      doBest = TRUE,
                      nodesize = 5,
                      importance=TRUE
)
importance(tRndFor_full)
varImpPlot(tRndFor_full)


###___________Variable Transformation__Bin the variable________________________##

dataset_mf_new$Age_bin <- cut(dataset_mf_new$Age,seq(0,90,10))
summary(dataset_mf_new$Age_bin)
write.csv(dataset_mf_new, "dataset_mf_new.csv")
dataset_mf_new <- read.csv("dataset_mf_new.csv", header = TRUE)
dataset_mf_new <- dataset_mf_new[ ,-c(1,25)]


#### -------------------------dataset for prediction ------------------------------------------
flight_data_new <- dataset_mf_new[,c(1:5,18:20,6)]
write.csv(flight_data_new, "flight_data_new.csv")
read.csv("flight_data_new.csv", header = TRUE)

###___________Variable Transformation__Bin the variable________________________##
flight_data_new1 <- flight_data_new
flight_data_new1$delay_in_min <-scale((flight_data_new1$DepartureDelayin_Mins+ flight_data_new1$ArrivalDelayin_Mins)/2)

hist(flight_data_new1$delay_in_min)

flight_data_new1$Age<- cut(flight_data_new1$Age,seq(0,90,10))
summary(flight_data_new1$DepartureDelayin_Mins)
summary(flight_data_new1$ArrivalDelayin_Mins)
flight_data_new2 <- flight_data_new1[,c(1:4,6,7,10,9)]

###------------------------------data split for model building -------------------------------------------------------
#install.packages("caTools")
library(caTools)
set.seed(123)

split = sample.split(flight_data_new$Satisfaction, SplitRatio = 0.8)
training_set = subset(flight_data_new, split == TRUE)
test_set = subset(flight_data_new, split == FALSE)

(sum(training_set$Satisfaction == "satisfied")/(sum(training_set$Satisfaction == "satisfied")+
                                                  sum(training_set$Satisfaction == "neutral or dissatisfied") ) ) *100

(sum(test_set$Satisfaction == "satisfied")/(sum(test_set$Satisfaction == "satisfied")+
                                              sum(test_set$Satisfaction == "neutral or dissatisfied") ) ) *100

table(training_set$Satisfaction)
table(test_set$Satisfaction)
###----------------glm regression--------------------
library(ROSE)
set.seed(seed)
mlr_glm <- glm(Satisfaction~., training_set[, 1:9], family = binomial)
summary(mlr_glm)
prob_mlr_train <- predict(mlr_glm, training_set[,1:8])
prob_mlr_train_prob <- predict(mlr_glm, training_set[,1:8], type = "response")
prob_mlr_train_factor <- factor(ifelse(prob_mlr_train > 0.5,"satisfied", "neutral or dissatisfied"))
confusionMatrix(prob_mlr_train_factor , training_set$Satisfaction)
roc.curve(prob_mlr_train_factor , training_set$Satisfaction)
prob_mlr <- predict(mlr_glm, test_set[,1:8])
prob_mlr_test <- predict(mlr_glm, test_set, type = "response")
prob_mlr_factor <- factor(ifelse(prob_mlr > 0.5,"satisfied", "neutral or dissatisfied"))
confusionMatrix(prob_mlr_factor, test_set$Satisfaction)
roc.curve(prob_mlr_factor, test_set$Satisfaction)


#install.packages("randomForest")
library(randomForest)
library(caret)
rf_model <- randomForest(Satisfaction~.,training_set[, c(1:9)])
rf_model
varImpPlot(rf_model)
rf_class = predict(rf_model, training_set[,1:8], type="class")
confusionMatrix(training_set$Satisfaction, rf_class)
roc.curve(training_set$Satisfaction, rf_class)
rf_class_test = predict(rf_model, test_set[,1:8], type="class")
confusionMatrix(test_set$Satisfaction, rf_class_test)
roc.curve(test_set$Satisfaction, rf_class_test)


set.seed(seed)
tRndFor = tuneRF(x = training_set[,1:8],
                 y=training_set$Satisfaction,
                 mtryStart = 3,
                 ntreeTry = 51,
                 stepFactor = 1.5,
                 improve = 0.0001,
                 trace=TRUE,
                 plot = TRUE,
                 doBest = TRUE,
                 nodesize = 5,
                 importance=TRUE
)
importance(tRndFor)
varImpPlot(tRndFor)

trndfor_predict <- predict(tRndFor, training_set[,1:8])
trndfor_prob1_rf_prob = predict(tRndFor, training_set[,1:8], type="prob")[,2]
confusionMatrix(training_set$Satisfaction, trndfor_predict)
roc.curve(trndfor_predict , training_set$Satisfaction)
#TEST data
test_set_predict.class_rf <- predict(tRndFor, test_set[,1:8], type="class")
test_set_prob1_rf <- predict(tRndFor, test_set[,1:8], type="prob")[,2]
confusion_matrix_rf <- confusionMatrix(test_set$Satisfaction, test_set_predict.class_rf)
confusion_matrix_rf
roc.curve(test_set$Satisfaction, test_set_predict.class_rf)

#knn
trctrl <- trainControl( method = "repeatedcv",number=10, repeats = 3 )
set.seed(3333)

knn_fit <- train(Satisfaction~., data = training_set[,1:9], method = "knn", trControl=trctrl)
knn_fit

plot(knn_fit)

train_knn <- predict(knn_fit, training_set[,1:8])
train_knn_prob <- predict(knn_fit, training_set[,1:8], type = "prob")[,2]
confusionMatrix(train_knn, training_set$Satisfaction)
roc.curve(train_knn, training_set$Satisfaction)
#test_knn
test_knn <- predict(knn_fit, newdata = test_set[,1:8])
test_knn_prob <- predict(knn_fit, test_set[,1:8], type = "prob")[,2]
confusionMatrix(test_knn, test_set$Satisfaction)
roc.curve(test_knn, test_set$Satisfaction)


#naive bayes 
xtrain <- training_set[,1:8]
ytrain <- training_set$Satisfaction
xtest <- test_set[,1:8]
ytest <- test_set$Satisfaction

library(e1071)
nb <- naiveBayes(xtrain, ytrain)
nb_train_pred<- predict(nb, xtrain)
nb_train_prob <- predict(nb, xtrain, type = "raw")[,2]
confusionMatrix(nb_train_pred, ytrain)
roc.curve(nb_train_pred, ytrain)
nb_pred <- predict(nb, xtest)
nb_test_prob <- predict(nb, xtest, type = "raw")[,2]
confusionMatrix(nb_pred, ytest)
roc.curve(nb_pred, ytest)

library(gbm)          # basic implementation using AdaBoost
library(xgboost)
set.seed(123)
training_set$Satisfaction1 <- ifelse(training_set$Satisfaction == "neutral or dissatisfied", 0,1)
test_set$Satisfaction1 <- ifelse(test_set$Satisfaction == "neutral or dissatisfied", 0,1)

mod_gbm1 = gbm(training_set$Satisfaction1~.,
               data = training_set[,1:8],
               distribution = "bernoulli",
               cv.folds = 5,
               n.minobsinnode = 5,
               shrinkage = .01,
               n.trees = 200, 
               interaction.depth = 2)
summary(mod_gbm1)
gbm_train_pred <- predict.gbm(mod_gbm1, training_set[,1:8], type = "response")
gbm_train_pred_factor <- factor(ifelse(gbm_train_pred > 0.5,"satisfied", "neutral or dissatisfied"))
confusionMatrix(gbm_train_pred_factor, training_set$Satisfaction)
roc.curve(gbm_train_pred_factor, training_set$Satisfaction)
gbm_test_pred <- predict(mod_gbm1, test_set[,1:8], type = "response")
gbm_test_pred_factor <- factor(ifelse(gbm_test_pred > 0.5,"satisfied", "neutral or dissatisfied"))
confusionMatrix(gbm_test_pred_factor, test_set$Satisfaction)
roc.curve(gbm_test_pred_factor, test_set$Satisfaction)



training_set$mlr_prob <- prob_mlr_train_prob 
training_set$rf_prob <- trndfor_prob1_rf_prob
training_set$knn_prob <- train_knn_prob
training_set$nb_prob <- nb_train_prob
training_set$gbm_prob <- gbm_train_pred
test_set$mlr_prob <- prob_mlr_test
test_set$rf_prob <- test_set_prob1_rf
test_set$knn_prob <- test_knn_prob
test_set$nb_prob <- nb_test_prob
test_set$gbm_prob <- gbm_test_pred

training_set <- training_set[,c(1:13,15,14)]
test_set <- test_set[,c(1:13,15,14)]

#########----------------Ensemble model with random forest as the top layer model------------------
set.seed(seed)
model_ens_rf <- randomForest(Satisfaction~., training_set[,c(9,11,13,14)])
model_ens_rf
rf_ens_train_pred <- predict(model_ens_rf, training_set[,c(11,13,14)])
confusionMatrix(rf_ens_train_pred, training_set$Satisfaction)
roc.curve(rf_ens_train_pred, training_set$Satisfaction)
rf_ens_test_pred <- predict(model_ens_rf, test_set[,c(11,13,14)])
confusionMatrix(rf_ens_test_pred, test_set$Satisfaction)
roc.curve(rf_ens_test_pred, test_set$Satisfaction)

####---------------ensemble with logistic regression as the top layer model------------
model_ens_glm <- glm(Satisfaction~., training_set[,c(9,11,13,14)], family = binomial)
train_ens_glm <- predict(model_ens_glm, training_set[,c(11,13,14)])
train_ens_glm_factor <- factor(ifelse(train_ens_glm > 0.5,"satisfied", "neutral or dissatisfied"))
confusionMatrix(train_ens_glm_factor, training_set$Satisfaction)
roc.curve(train_ens_glm_factor, training_set$Satisfaction)
test_ens_glm <- predict(model_ens_glm, test_set[,c(11,13,14)])
test_ens_glm_factor <- factor(ifelse(test_ens_glm > 0.5,"satisfied", "neutral or dissatisfied"))
confusionMatrix(test_ens_glm_factor, test_set$Satisfaction)
roc.curve(test_ens_glm_factor, test_set$Satisfaction)

####---------------ensemble with Gradient Boosting as the top layer model------------
set.seed(seed)
mod_ens_gbm1 = gbm(training_set$Satisfaction1~.,
                   data = training_set[,c(11,13,14)],
                   distribution = "bernoulli",
                   cv.folds = 3,
                   n.minobsinnode = 5,
                   shrinkage = .01,
                   n.trees = 200, 
                   interaction.depth = 2)

summary(mod_ens_gbm1)
gbm_ens_train_pred <- predict.gbm(mod_ens_gbm1, training_set[,c(11,13,14)], type = "response")
gbm_ens_train_pred_factor <- factor(ifelse(gbm_ens_train_pred > 0.5,"satisfied", "neutral or dissatisfied"))
confusionMatrix(gbm_ens_train_pred_factor, training_set$Satisfaction)
roc.curve(gbm_ens_train_pred_factor, training_set$Satisfaction)
gbm_ens_test_pred <- predict(mod_ens_gbm1, test_set[,c(11,13,14)], type = "response")
gbm_ens_test_pred_factor <- factor(ifelse(gbm_ens_test_pred > 0.5,"satisfied", "neutral or dissatisfied"))
confusionMatrix(gbm_ens_test_pred_factor, test_set$Satisfaction)
roc.curve(gbm_ens_test_pred_factor, test_set$Satisfaction)
