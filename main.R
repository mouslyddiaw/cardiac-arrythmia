### Cardiac arrhythmia classification
rm(list=ls()) 
library("easypackages")  
libraries("readr","party","leaps","caret","Boruta")


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

###############################################
#Feature selection 
set.seed(111)
boruta.arrhythmia <- TentativeRoughFix(Boruta(diagnosis~., data =  arrhythmia, doTrace = 2)) 

getSelectedAttributes(boruta.arrhythmia, withTentative = F)

#lda, lasso, random forest
###############################################
#Dimension reduction
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(diagnosis ~., data = arrhythmia,
                    method = "pls",
                    preProc = c("center", "scale"), 
                    tuneLength = 15,
                    trControl = train.control
) 
step.model$bestTune

summary(step.model$finalModel)

#cf1 <- cforest(diagnosis ~ . , data= arrhythmia, control=cforest_unbiased(mtry=2,ntree=50)) # fit the random forest
#varimp(cf1)
