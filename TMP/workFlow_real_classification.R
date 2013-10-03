Model.type = c("Lasso","Ridge","ENet","RF","SVM")
Threshold.Method = c("MEAN","ThirtySixty","GMM")

source("~/AMGEN_BSEP/R/binarization.R")
source("~/AMGEN_BSEP/myREAL_classification.R")
source("~/AMGEN_BSEP/myData.R")
source("~/AMGEN_BSEP/R/Analysis_test_AUC.R")

synXXX<-"syn2271321"
synYYY<-"syn2271347"
testXXX<-"syn2271323"

dataset<-myData(synXXX,synYYY)

resultsRealCat.enet <- myREAL_classification(synXXX,synYYY,synTest,model.type = "ENet", thresholdMethod= "MEAN",nfolds = 5)    
resultsRealCat.lasso <- myREAL_classification(synXXX,synYYY,synTest,model.type = "Lasso", thresholdMethod= "MEAN",nfolds = 5)    
resultsRealCat.ridge <- myREAL_classification(synXXX,synYYY,synTest,model.type = "Ridge", thresholdMethod= "MEAN",nfolds = 5)    
resultsRealCat.rf <- myREAL_classification(synXXX,synYYY,synTest,model.type = "RF", thresholdMethod= "MEAN",nfolds = 5)    
resultsRealCat.svm <- myREAL_classification(synXXX,synYYY,synTest,model.type = "SVM", thresholdMethod= "MEAN",nfolds = 5)    

test_response<-loadEntity("syn2271345")
test.response<-test_response$objects[[1]]
a2<-which(is.na(test.response))

# after following process, type the results to check the ROC
Analysis.enet<-Analysis_test_AUC(resultsRealCat.enet[-a2,1],test.response[-a2])
Analysis.lasso<-Analysis_test_AUC(resultsRealCat.lasso[-a2,1],test.response[-a2])
Analysis.ridge<-Analysis_test_AUC(resultsRealCat.ridge[-a2,1],test.response[-a2])
Analysis.rf<-Analysis_test_AUC(resultsRealCat.rf[-a2,1],test.response[-a2])
Analysis.svm<-Analysis_test_AUC(resultsRealCat.svm[-a2,1],test.response[-a2])