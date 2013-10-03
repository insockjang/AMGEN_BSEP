Model.type = c("Ridge","Lasso","ENet")
source("~/AMGEN_BSEP/R/binarization.R")
source("~/AMGEN_BSEP/myBSModel_classification.R")
source("~/AMGEN_BSEP/myData.R")

synXXX<-"syn2271321"
synYYY<-"syn2271347"

dataset<-myData(synXXX,synYYY)
Penalty <- colnames(dataset$featureData)[sample(1:ncol(dataset$featureData), 10)]

resultsScale<-list()

k=2
resultsScale <- myBSModel_classification(synXXX,synYYY,model.type = Model.type[k], numBS= 5, numCore = 5, penaltys= Penalty)    
resultsScale.null <- myBSModel_classification(synXXX,synYYY,model.type = Model.type[k], numBS= 5, numCore = 5,penaltys = NULL)    

