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

resultsScale <- myREAL_classification(synXXX,synYYY,synTest,model.type = model.Type, thresholdMethod= "MEAN",nfolds = 5)    

test_response<-loadEntity("syn2271345")
test.response<-test_response$objects[[1]]

a2<-which(is.na(test.response))
Analysis<-Analysis_test_AUC(resultsScale[-a2,1],test.response[-a2])