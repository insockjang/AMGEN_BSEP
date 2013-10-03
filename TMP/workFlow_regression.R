Model.type = c("Lasso","Ridge","ENet","RF","SVM")


source("~/AMGEN_BSEP/myModel_regression.R")
source("~/AMGEN_BSEP/myData.R")

synXXX<-"syn2271321"
synYYY<-"syn2271333"
dataset<-myData(synXXX,synYYY)

Penalty <- colnames(dataset$featureData)[sample(1:ncol(dataset$featureData), 10)]

model.Type = "Lasso"
resultsScale <- myModel_regression(synXXX,synYYY, model.type = model.Type, nfolds = 5, penaltys= Penalty)    
resultsScale.null <- myModel_regression(synXXX,synYYY, model.type = model.Type, nfolds = 5,penaltys= NULL)    
