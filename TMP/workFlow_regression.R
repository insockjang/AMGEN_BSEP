Model.type = c("Lasso","Ridge","ENet","RF","SVM")


source("~/AMGEN_BSEP/myModel_regression.R")
source("~/AMGEN_BSEP/myData.R")

for(model.Type in Model.type){  
  filename = paste("~/AMGEN_BSEP/TMP/newPredictiveModel_", model.Type,"_regression_",Threshold.Method,".Rdata",sep="")
  if(!file.exists(filename)){        
    resultsScale <- myModel_regression(model.type = model.Type, nfolds = 5,ThresholdMethod = Threshold.Method)    
    save(resultsScale,file = filename)        
  }  
}
