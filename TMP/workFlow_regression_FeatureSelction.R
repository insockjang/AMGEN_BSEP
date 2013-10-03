Model.type = c("Ridge","Lasso","ENet")

source("~/AMGEN_BSEP/myBSModel_regression.R")
source("~/AMGEN_BSEP/myData.R")

synXXX<-"syn2271321"
synYYY<-"syn2271333"

dataset<-myData(synXXX,synYYY)
Penalty <- colnames(dataset$featureData)[sample(1:ncol(dataset$featureData), 10)]


resultsScale <- myBSModel_regression(synXXX,synYYY, model.type = model.Type, numBS= 5, numCore = 5, penaltys= Penalty)
resultsScale.null <- myBSModel_regression(synXXX,synYYY, model.type = model.Type, numBS= 5, numCore = 5, penaltys= NULL)
