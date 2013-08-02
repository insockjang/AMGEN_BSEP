Model.type = c("Ridge","Lasso","ENet")

source("~/AMGEN_BSEP/myBSModel_classification.R")
source("~/AMGEN_BSEP/myData.R")

resultsScale<-list()

for(k in 1:length(Model.type)){
  resultsScale[[k]] <- myBSModel_classification(model.type = Model.Type[k], numBS= 100, numCore = 5)    
}
names(resultsScale)<-Model.type