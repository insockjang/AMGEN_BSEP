Model.type = c("Lasso","Ridge","ENet","RF","SVM")
Threshold.Method = c("Mean","ThirtySixty","GMM")

source("~/AMGEN_BSEP/R/binarization.R")
source("~/AMGEN_BSEP/myModel_classification.R")
source("~/AMGEN_BSEP/myData.R")

synXXX<-"syn2271321"
synYYY<-"syn2271347"

dataset<-myData(synXXX,synYYY)

Penalty <- colnames(dataset$featureData)[sample(1:ncol(dataset$featureData), 10)]

# ENet Model
resultsScale.enet <- myModel_classification(synXXX,synYYY,model.type = "ENet", nfolds = 5,penaltys= Penalty)    
resultsScale.null.enet <- myModel_classification(synXXX,synYYY,model.type = "ENet", nfolds = 5,penaltys= NULL)    

# Lasso Model
resultsScale.lasso <- myModel_classification(synXXX,synYYY,model.type = "Lasso", nfolds = 5,penaltys= Penalty)    
resultsScale.null.lasso <- myModel_classification(synXXX,synYYY,model.type = "Lasso", nfolds = 5,penaltys= NULL)    

# Ridge Model
resultsScale.ridge <- myModel_classification(synXXX,synYYY,model.type = "Ridge", nfolds = 5)    

# RF Model
resultsScale.rf <- myModel_classification(synXXX,synYYY,model.type = "RF", nfolds = 5)    

# SVM Model
resultsScale.svm <- myModel_classification(synXXX,synYYY,model.type = "SVM", nfolds = 5)    
