#### Cardiac arrhythmia classification
rm(list=ls()) 
library("easypackages")  
libraries("readr","party","leaps","caret","pROC","Boruta")

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

#pairs(arrhythmia[,c(1:6,14,143)], col=arrhythmia$diagnosis)

predictors <- arrhythmia[,-length(arrhythmia)]
class <- arrhythmia$diagnosis

#Data splitting 
set.seed(1) 
inTrain <- createDataPartition(class, p = .7)[[1]] 

arrhythmiaTrain <-  arrhythmia[inTrain, ]
arrhythmiaTest <-  arrhythmia[-inTrain, ]

###############################################
### Building classifiers 
## Logistic regression
set.seed(1056)
logReg <- train(diagnosis ~ .,
                     data = arrhythmiaTrain,
                     method = "glm",
                     family=binomial(),
                     metric = "ROC",
                     trControl = trainControl(method = "repeatedcv",
                                              repeats = 5,classProbs = TRUE))

varImp(logReg)

logRegPred <- predict(logReg, arrhythmiaTest)  

confusionMatrix(logRegPred,arrhythmiaTest$diagnosis,positive = "Anormal") 

## Logistic regression regularized
set.seed(1056)
logRegPen <- train(diagnosis ~ .,
                data = arrhythmiaTrain,
                method = "glmnet",
                family=binomial(),
                metric = "ROC",
                trControl = trainControl(method = "repeatedcv",
                                         repeats = 5,classProbs = TRUE))


# alpha = 1, 
# lambda = NULL,

## PLS

## LDA

## sparse LDA (penalized)

## Linear SVM  

## Polynomial SVM 

## Radial SVM  
set.seed(1056) 

svmFit <- train(diagnosis ~ .,
                data = arrhythmiaTrain,
                method = "svmRadial",
                preProc = c("center", "scale"),
                tuneLength = 10,
                metric = "ROC",
                trControl = trainControl(method = "repeatedcv",
                                         repeats = 5,classProbs = TRUE))

ggplot(svmFit) + theme_bw()

svmPred <- predict(svmFit, arrhythmiaTest)  

confusionMatrix(svmPred,arrhythmiaTest$diagnosis,positive = "Anormal") 

## kNN
set.seed(1056) 
knnModel = train(
  diagnosis ~ .,
  data = arrhythmiaTrain,
  method = "knn",
  metric = "ROC",
  trControl = trainControl(method = "repeatedcv",
                           repeats = 5,classProbs = TRUE),
  tuneGrid = expand.grid(k = seq(1, 101, by = 2)) #optional
  )
  

ggplot(knnModel) + theme_bw()
  
knnPred <- predict(knnModel, arrhythmiaTest)  
  
confusionMatrix(knnPred,arrhythmiaTest$diagnosis,positive = "Anormal") 

## CART 

## Random forests
set.seed(1056)
tunegrid <- expand.grid(.mtry=c(1:15))
rfModel <- train(
  diagnosis~., 
  data=arrhythmiaTrain, 
  method="rf", 
  metric="ROC",
  tuneGrid=tunegrid, 
  trControl=trainControl(method = "repeatedcv",
                         repeats = 5,classProbs = TRUE))
ggplot(rfModel) + theme_bw()

rfPred <- predict(rfModel, arrhythmiaTest)  

confusionMatrix(rfPred,arrhythmiaTest$diagnosis,positive = "Anormal") 

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

resamp <- resamples(list(SVM = svmFit, Logistic = logReg, kNN = knnModel))
summary(resamp)

modelDifferences <- diff(resamp)
summary(modelDifferences)


# trellis.par.set(caretTheme())
# dotplot(resamps, metric = "ROC")


# theme1 <- trellis.par.get()
# theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
# theme1$plot.symbol$pch = 16
# theme1$plot.line$col = rgb(1, 0, 0, .7)
# theme1$plot.line$lwd <- 2
# trellis.par.set(theme1)
# bwplot(resamps, layout = c(3, 1))
