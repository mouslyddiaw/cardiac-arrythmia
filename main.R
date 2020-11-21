#### Cardiac arrhythmia classification
rm(list=ls()) 
library("easypackages")  
libraries("readr","party","leaps","caret","pROC","MLmetrics","glmnet")

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

f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  c(F1 = f1_val)
}

ctrl <- trainControl(method = "repeatedcv", repeats = 5,classProbs = TRUE,summaryFunction = f1)

## Logistic regression

# Specify a null model with no predictors
null_model <- glm(diagnosis ~ 1, data = arrhythmiaTrain,family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(diagnosis ~ ., data = arrhythmiaTrain,family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

set.seed(1056)
logReg <- train(diagnosis ~ HT + qrs_duration + DD + DA + KK + 
                  EN + HA + KT + KG + height + BN + GP + FB + AV + CL + DP + 
                  DL + JG + GM + JL + IT,
                data = arrhythmiaTrain,
                method = "glm",
                family=binomial,
                metric = "F1",   
                trControl = ctrl)  

## PLS
plsFit <- train(diagnosis ~ .,
                data = arrhythmiaTrain,
                method = "pls",
                tuneGrid = expand.grid(.ncomp = 1:10),
                preProc = c("center","scale"),
                metric = "F1",
                trControl = ctrl)

varImp(plsFit, scale = FALSE)
plot(varImp(plsFit, scale = FALSE), top = 20, scales = list(y = list(cex = .95)))

## LDA
set.seed(1056)
ldaFit <- train(diagnosis ~ .,
                data = arrhythmiaTrain,
                method = "lda", 
                preProcess = c("center", "scale"),
                metric = "F1",
                trControl = ctrl 
)

set.seed(1056)
ldaFit2 <- train(diagnosis ~ .,
                 data = arrhythmiaTrain,
                 method = "lda", 
                 preProcess = c("center", "scale","pca"),
                 metric = "F1",
                 trControl = ctrl
)

## sparse LDA 
set.seed(1056)
sparseldaFit <- train(diagnosis ~ .,
                data = arrhythmiaTrain,
                method = "sparseLDA", 
                preProcess = c("center", "scale"),
                metric = "F1",
                trControl = ctrl
)

## Linear SVM  
set.seed(1056) 

svmLin <- train(diagnosis ~ .,
                data = arrhythmiaTrain,
                method = "svmLinear",
                preProc = c("center", "scale"),
                tuneLength = 10,
                metric = "F1",
                trControl = ctrl)

## Radial SVM  
set.seed(1056) 

svmRad <- train(diagnosis ~ .,
                data = arrhythmiaTrain,
                method = "svmRadial",
                preProc = c("center", "scale"),
                tuneLength = 10,
                metric = "F1",
                trControl = ctrl)

ggplot(svmFit) + theme_bw() 

## Polynomial SVM 
set.seed(1056) 

svmPol<- train(diagnosis ~ .,
               data = arrhythmiaTrain,
               method = "svmPoly",
               preProc = c("center", "scale"),
               tuneLength = 4,
               metric = "F1",
               trControl = ctrl)

## kNN
set.seed(1056) 
knnModel = train(
  diagnosis ~ .,
  data = arrhythmiaTrain,
  method = "knn",
  metric = "F1",
  trControl = ctrl,
  preProc = c("center", "scale", "pca"),
  tuneGrid = expand.grid(k = seq(1, 101, by = 2))  
  )
  

ggplot(knnModel) + theme_bw() 
                   
## Random forests
set.seed(1056) 
rfModel <- train(
  diagnosis~., 
  data=arrhythmiaTrain, 
  method="rf", 
  metric="F1",
  tuneGrid= expand.grid(.mtry=c(1:15)), 
  trControl=ctrl)

ggplot(rfModel) + theme_bw() 

## CART 
rpFit <- train(
  diagnosis ~ .,
  data = arrhythmiaTrain,
  method = "rpart",
  metric = "F1",
  # maximize = FALSE,
  # tuneLength = 20,
  trControl = ctrl)

## Bagging

set.seed(1056) 
bagModel <- train(
  diagnosis~., 
  data=arrhythmiaTrain, 
  method= "treebag", 
  metric="F1", 
  trControl=ctrl) 

###############################################
### Results summary

results <- resamples(list(Logistic = logReg, Logistic2 = logReg2,
                          PenalizedLogistic = logRegPen, 
                          PLS = plsFit, LDA = ldaFit, LDA2 = ldaFit2,
                          sparseLDA= sparseldaFit, SVMLin=svmLin,SVMRad=svmRad,
                          SVMPoly = svmPol,kNN = knnModel, RF = rfModel)) #, CART = rpFit,

dotplot(results) 

summary(results)

bwplot(results)
 
  

xyplot(results)
densityplot(results)



modelDifferences <- diff(results)
summary(modelDifferences)

###############################################
### Prediction results
finalmodel <- logReg
predProb <- predict(finalmodel , arrhythmiaTest,type = "prob")
predClass <- predict(finalmodel , arrhythmiaTest)

confusionMatrix(predClass,arrhythmiaTest$diagnosis,positive = "Anormal") 
F1_Score(y_pred = predClass, y_true = arrhythmiaTest$diagnosis, positive = "Anormal")


auc( roc(response = arrhythmiaTest$diagnosis,
                     predictor = predProb[,"Anormal"],
                    levels = rev(levels(arrhythmiaTest$diagnosis))))
 
