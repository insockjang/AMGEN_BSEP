Model.type = c("Ridge","Lasso","ENet")
source("~/AMGEN_BSEP/R/binarization.R")
source("~/AMGEN_BSEP/myBSModel_classification.R")
source("~/AMGEN_BSEP/myData.R")

resultsScale<-list()
Penalty <- colnames(filteredFeatureDataScaled)[sample(1:ncol(filteredFeatureDataScaled), 10)]

k=2
resultsScale <- myBSModel_classification(synXXX,synYYY,model.type = Model.type[k], numBS= 5, numCore = 5, penaltys= Penalty)    
resultsScale.null <- myBSModel_classification(synXXX,synYYY,model.type = Model.type[k], numBS= 5, numCore = 5,penaltys = NULL)    

