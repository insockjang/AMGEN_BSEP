Model.type = c("Lasso","Ridge","ENet","RF","SVM")


source("~/AMGEN_BSEP/myREAL_regression.R")
source("~/AMGEN_BSEP/myData.R")

synXXX<-"syn2271321"
synYYY<-"syn2271333"
testXXX<-"syn2271323"

dataset<-myData(synXXX,synYYY)

resultsScale <- myREAL_regression(synXXX,synYYY,testXXX,model.type = model.Type,nfolds = 5)    

test_response<-loadEntity("syn2271345")
test.response<-test_response$objects[[1]]

a2<-which(is.na(test.response))
Analysis<-Analysis_test_COR(resultsScale[-a2,1],test.response[-a2])