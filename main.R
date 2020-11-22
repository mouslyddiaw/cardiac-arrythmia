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

#Final dataframe
arrhythmia  <- subset(arrhythmia, select = -c(II,IO) ) 
numerical_cols <- setdiff(numerical_cols,c("II","IO"))
colnames(arrhythmia) <- make.names(colnames(arrhythmia)) 

ggplot(arrhythmia, aes(x=qrs_duration , y= heart_rate,color=diagnosis)) +
  geom_point() + stat_ellipse() + theme_minimal() + theme(text = element_text(size=20))

#Data splitting 
predictors <- arrhythmia[,-length(arrhythmia)]
class <- arrhythmia$diagnosis

set.seed(1) 
inTrain <- createDataPartition(class, p = .7)[[1]] 
arrhythmiaTrain <-  arrhythmia[inTrain, ]
arrhythmiaTest <-  arrhythmia[-inTrain, ]

###############################################
### Building classifiers 

#F1-score custom metric
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  c(F1 = f1_val)
}

ctrl <- trainControl(method = "repeatedcv", repeats = 5,classProbs = TRUE,summaryFunction = f1)

## Stepwise Logistic regression

# Specify a null model with no predictors
null_model <- glm(diagnosis ~ 1, data = arrhythmiaTrain,family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(diagnosis ~ ., data = arrhythmiaTrain,family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

#Tune logistic regression 
set.seed(1056)
logReg <- train(diagnosis ~ HT + qrs_duration + DD + DA + KK + 
                  EN + HA + KT + KG + height + BN + GP + FB + AV + CL + DP + 
                  DL + JG + GM + JL + IT,
                data = arrhythmiaTrain,
                method = "glm",
                family=binomial,
                preProc = c("center","scale"),
                metric = "F1",  
                trControl = ctrl)  
 
plot(varImp(logReg, scale = FALSE), top = 20, scales = list(y = list(cex = 1.5)))  

## PLS
set.seed(1056)
plsFit <- train(diagnosis ~ .,
                data = arrhythmiaTrain,
                method = "pls",
                tuneGrid = expand.grid(.ncomp = 1:10),
                preProc = c("center","scale"),
                metric = "F1",
                trControl = ctrl)
 
## LDA
set.seed(1056)
ldaFit <- train(diagnosis ~ .,
                data = arrhythmiaTrain,
                method = "lda", 
                preProcess = c("center", "scale"),
                metric = "F1",
                trControl = ctrl 
)
  

## PCA + LDA
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

ggplot(svmRad) + theme_bw()  + theme(text = element_text(size=17.5))  

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
  preProc = c("center", "scale"),
  tuneGrid = expand.grid(k = seq(1, 101, by = 2))  
  )
   
                   
## Random forests
set.seed(1056) 
rfModel <- train(
  diagnosis~., 
  data=arrhythmiaTrain, 
  method="rf", 
  metric="F1",
  tuneGrid= expand.grid(.mtry=c(1:15)), 
  trControl=ctrl)

ggplot(rfModel) + theme_bw()   + theme(text = element_text(size=17.5))  
plot(varImp(rfModel, scale = FALSE), top = 20, scales = list(y = list(cex = 0.95)))

## CART 
set.seed(1056) 
rpFit <- train(
  diagnosis ~ .,
  data = arrhythmiaTrain,
  method = "rpart2",
  metric = "F1", 
  tuneLength = 10,
  trControl = ctrl) 

#plot(rpFit$finalModel)

## Boosted tree
gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                       n.trees = seq(100, 1000, by = 50),
                       shrinkage = c(0.01, 0.1),
                       n.minobsinnode = 10)

set.seed(1056) 
gbmModel <- train(
  diagnosis ~ .,
  data = arrhythmiaTrain, 
  method = "gbm",
  tuneGrid = gbmGrid, 
  metric = "F1",
  verbose = FALSE,
  trControl = ctrl)

ggplot(gbmModel) + theme_bw()  + theme(text = element_text(size=17.5))  

###############################################
### Results summary

results <- resamples(list(LogisticReg = logReg, 
                          PLS = plsFit, LDA = ldaFit, pca_LDA = ldaFit2,
                          sparseLDA= sparseldaFit, LinearSVM=svmLin,RadialSVM=svmRad,
                          PolySVM = svmPol, kNN = knnModel, RandForest = rfModel,
                          CART = rpFit, GBM= gbmModel)) 

# summary(results)
# dotplot(results) 

bwplot(results)
 
xyplot(resamples(list(LogisticReg = logReg,  GBM= gbmModel))) 

# densityplot(resamples(list(LogisticReg = logReg,  RandForest = rfModel,
#                            GBM= gbmModel)),auto.key = TRUE)


modelDifferences <- diff(resamples(list(LogisticReg = logReg,  RandForest = rfModel,
                                        GBM= gbmModel)))
summary(modelDifferences)

###############################################
### Prediction results

#Performance of Log. Reg.
predProb1 <- predict(logReg , arrhythmiaTest,type = "prob")
predClass1 <- predict(logReg , arrhythmiaTest)

confusionMatrix(predClass1,arrhythmiaTest$diagnosis,positive = "Anormal") 
f1_1 <- F1_Score(y_pred = predClass1, y_true = arrhythmiaTest$diagnosis, positive = "Anormal")

rocCurve1 <- roc(response = arrhythmiaTest$diagnosis,
            predictor = predProb1$Anormal,
            levels = rev(levels(arrhythmiaTest$diagnosis)))

auc1 <- auc( rocCurve1)

#Performance of RF
predProb2 <- predict(rfModel , arrhythmiaTest,type = "prob")
predClass2 <- predict(rfModel , arrhythmiaTest)

confusionMatrix(predClass2,arrhythmiaTest$diagnosis,positive = "Anormal") 
f1_2 <- F1_Score(y_pred = predClass2, y_true = arrhythmiaTest$diagnosis, positive = "Anormal")

rocCurve2 <- roc(response = arrhythmiaTest$diagnosis,
                 predictor = predProb2$Anormal,
                 levels = rev(levels(arrhythmiaTest$diagnosis)))

auc2 <- auc( rocCurve2) 

plot(rocCurve1, col = "black", lty = 2)
plot(rocCurve2, add = TRUE, col = "blue")
legend(0.8, 0.2, legend = c("Logistic Regression", "Random Forest"),
       col = c("black", "blue"), lty = 2:1, cex = 0.95)
 
