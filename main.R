### Cardiac arrhythmia classification
rm(list=ls()) 
library(readr)

#Import data processed in jupyter notebook
arrhythmia <- read_delim("Desktop/cardiac_arrythmia/data_arrhythmia_preprocess.csv", ";")

#Get categorical and numerical variables
categorical_cols <- c()
for(col in names(arrhythmia )){
  if (length(unique(arrhythmia[[col]])) == 2){
     categorical_cols <- append(categorical_cols,col) 
     #change type in r  
  }
}

numerical_cols <- setdiff(names(arrhythmia ),categorical_cols)

#Correlated numerical variables


