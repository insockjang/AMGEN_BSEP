Model.type = c("Lasso","Ridge","ENet","RF","SVM")


source("~/AMGEN_BSEP/myREAL_regression.R")
source("~/AMGEN_BSEP/myData.R")

synXXX<-"syn2271321"
synYYY<-"syn2271333"
testXXX<-"syn2271323"

dataset<-myData(synXXX,synYYY)

resultsScale <- myREAL_regression(synXXX,synYYY,testXXX,model.type = model.Type, nfolds = 5)    

resultsRealReg.enet <- myREAL_regression(synXXX,synYYY,synTest,model.type = "ENet", nfolds = 5)    
resultsRealReg.lasso <- myREAL_regression(synXXX,synYYY,synTest,model.type = "Lasso", nfolds = 5)    
resultsRealReg.ridge <- myREAL_regression(synXXX,synYYY,synTest,model.type = "Ridge", nfolds = 5)    
resultsRealReg.rf <- myREAL_regression(synXXX,synYYY,synTest,model.type = "RF", nfolds = 5)    
resultsRealReg.svm <- myREAL_regression(synXXX,synYYY,synTest,model.type = "SVM", nfolds = 5)    

test_response<-loadEntity("syn2271335")
test.response<-test_response$objects[[1]]
a2<-which(is.na(test.response))

cor(resultsRealReg.enet[-a2,1],test.response[-a2])
cor(resultsRealReg.lasso[-a2,1],test.response[-a2])
cor(resultsRealReg.ridge[-a2,1],test.response[-a2])
cor(resultsRealReg.rf[-a2,1],test.response[-a2])
cor(resultsRealReg.svm[-a2,1],test.response[-a2])