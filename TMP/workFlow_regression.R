Model.type = c("Lasso","Ridge","ENet","RF","SVM")


source("~/AMGEN_BSEP/myModel_regression.R")
source("~/AMGEN_BSEP/myData.R")

synXXX<-"syn2271321"
synYYY<-"syn2271333"
dataset<-myData(synXXX,synYYY)

Penalty <- colnames(dataset$featureData)[sample(1:ncol(dataset$featureData), 10)]

# ENet Model
resultsScale.enet <- myModel_regression(synXXX,synYYY,model.type = "ENet", nfolds = 5,penaltys= Penalty)    
resultsScale.null.enet <- myModel_regression(synXXX,synYYY,model.type = "ENet", nfolds = 5,penaltys= NULL)    

# Lasso Model
resultsScale.lasso <- myModel_regression(synXXX,synYYY,model.type = "Lasso", nfolds = 5,penaltys= Penalty)    
resultsScale.null.lasso <- myModel_regression(synXXX,synYYY,model.type = "Lasso", nfolds = 5,penaltys= NULL)    

# Ridge Model
resultsScale.ridge <- myModel_regression(synXXX,synYYY,model.type = "Ridge", nfolds = 5)    

# RF Model
resultsScale.rf <- myModel_regression(synXXX,synYYY,model.type = "RF", nfolds = 5)    

# SVM Model
resultsScale.svm <- myModel_regression(synXXX,synYYY,model.type = "SVM", nfolds = 5)    
