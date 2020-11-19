#### Cardiac arrhythmia classification
rm(list=ls()) 
library("easypackages")  
libraries("readr","party","leaps","caret","pROC")

############################################### 
### Additional processing  

#Import data processed in jupyter notebook
arrhythmia <- read_delim("Desktop/cardiac_arrythmia/data_arrhythmia_preprocess.csv", ";")


#Get categorical and numerical variables
categorical_cols <- c()
for(col in names(arrhythmia )){
  if (length(unique(arrhythmia[[col]])) == 2){
     categorical_cols <- append(categorical_cols,col) 
     arrhythmia[[col]] <- as.factor(arrhythmia[[col]])  
  }
}

numerical_cols <- setdiff(names(arrhythmia ),categorical_cols)

#Drop highly correlated numerical variables
corrmat<- cor(arrhythmia[,numerical_cols])

highcorrnum <- data.frame()

for (i in seq(nrow(corrmat))){
  for (j in seq(ncol(corrmat))){
    if (corrmat[i,j]>0.9 & i!=j){
      highcorrnum <- rbind(highcorrnum, c(rownames(corrmat)[i],colnames(corrmat)[j]))
    }
  }
}

highcorrnum <- highcorrnum[!duplicated(t(apply(highcorrnum, 1, sort))), ] #Removing duplicates
#df = arrhythmia[,c("GO","II")]
#m = as.matrix(df)
#corPlot(m, method = "spearman")

arrhythmia  <- subset(arrhythmia, select = -c(II,IO) )
numerical_cols <- setdiff(numerical_cols,c("II","IO"))

predictors <- arrhythmia[,-length(arrhythmia)]
class <- arrhythmia$diagnosis

#Data splitting 
set.seed(1) 
inTrain <- createDataPartition(class, p = .8)[[1]] 

arrhythmiaTrain <-  arrhythmia[inTrain, ]
arrhythmiaTest <-  arrhythmia[-inTrain, ]

###############################################
### Building classifiers

## Logistic regression
set.seed(1056)
logisticReg <- train(diagnosis ~ .,
                     data = arrhythmiaTrain,
                     method = "glm",
                     metric = "ROC",
                     trControl = ctrl)

resamp <- resamples(list(SVM = svmFit, Logistic = logisticReg)) 
summary(resamp)

modelDifferences <- diff(resamp) 
summary(modelDifferences)

arrhythmiaTest$svmDiagnosis <- predict(svmFit, arrhythmiaTest)  

confusionMatrix(data = arrhythmiaTest$svmDiagnosis,
                reference = arrhythmiaTest$diagnosis,
                positive = "Normal") 

## Logistic regression regularized

## PLS

## LDA

## sparse LDA (penalized)

## Linear SVM  

## Polynomial SVM 

## Radial SVM  
set.seed(1056)
ctrl<- trainControl(method = "repeatedcv",
             repeats = 5,classProbs = TRUE)

svmFit <- train(diagnosis ~ .,
                data = arrhythmiaTrain,
                method = "svmRadial",
                preProc = c("center", "scale"),
                tuneLength = 10,
                metric = "ROC",
                trControl = ctrl)

## kNN

## CART 

## Random forests

## Bagging

 
# rocCurve <- roc(response = arrhythmiaTest$diagnosis,
#                 predictor = arrhythmiaTest$qrs_duration, 
#                 levels = rev(levels(arrhythmiaTest$diagnosis)))
# 
# auc(rocCurve)
# ci.roc(rocCurve)
# plot(rocCurve, legacy.axes = TRUE)

###############################################
### Results summary

