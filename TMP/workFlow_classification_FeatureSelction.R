Model.type = c("Ridge","Lasso","ENet")

source("~/AMGEN_BSEP/R/binarization.R")
source("~/AMGEN_BSEP/myBSModel_classification.R")
source("~/AMGEN_BSEP/myData.R")

synXXX<-"syn2271321"
synYYY<-"syn2271347"

dataset<-myData(synXXX,synYYY)
Penalty <- colnames(dataset$featureData)[sample(1:ncol(dataset$featureData), 10)]

# ENet Model
bsResultsScale.enet <- myBSModel_classification(synXXX,synYYY,model.type = "ENet", numBS= 5, numCore = 5, penaltys= Penalty)    
bsResultsScale.null.enet <- myBSModel_classification(synXXX,synYYY,model.type = "ENet", numBS= 5, numCore = 5, penaltys= NULL)    

# Lasso Model
bsResultsScale.lasso <- myBSModel_classification(synXXX,synYYY,model.type = "Lasso", numBS= 5, numCore = 5, penaltys= Penalty)    
bsResultsScale.null.lasso <- myBSModel_classification(synXXX,synYYY,model.type = "Lasso", numBS= 5, numCore = 5, penaltys= NULL)    

# Ridge Model
bsResultsScale.ridge <- myBSModel_classification(synXXX,synYYY,model.type = "Ridge", numBS= 5, numCore = 5)    

