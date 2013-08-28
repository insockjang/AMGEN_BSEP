require(glmnet)
myEnetModel_classification <- setRefClass(Class = "myEnetModel_classification",
                              contains="PredictiveModel",
                              fields="model",
                              methods = list(
                                initialize = function(...){
                                  return(.self)
                                },
                                
                                rawModel = function(){
                                  return(.self$model)
                                },
                                
                                customTrain = function(featureData, responseData, alpha = alpha, nfolds = 5, Penalty,...){
                                  if(!is.factor(responseData)){                              
                                    stop('Response variable should be two levels of factor')
                                  }   
                                  
                                  K<-which(!is.na(responseData))
                                  FeatureData<-featureData[K,]
                                  ResponseData<-responseData[K]
                                  
                                  if(!is.null(Penalty)){
                                    kk<-match(Penalty, colnames(FeatureData))
                                    R<-rep(1,ncol(FeatureData))
                                    R[kk]<-0
                                  }else{
                                    R<-rep(1,ncol(FeatureData))
                                  }
                                  
                                  if(length(alpha)==1){
                                    .self$model <- cv.glmnet(FeatureData,ResponseData, family = "binomial",alpha = alpha, nfolds = nfolds, penalty.factor = R,...) 
                                    optParam <- c(.self$model$cvm[which.min(.self$model$cvm)],alpha,.self$model$lambda[which.min(.self$model$cvm)])
                                    names(optParam)<-c("BinomialDeviance","alpha","lambdaOpt")
                                    .self$model$optParam <- optParam
                                  }
                                  else{
                                    optParam <-c()
                                    fit <-list()
                                    for(k in 1:length(alpha)){
                                      fit[[k]]<-cv.glmnet(FeatureData,ResponseData, family = "binomial", alpha = alpha[k], nfolds = nfolds,penalty.factor = R,...) 
                                      optParam<-rbind(optParam,c(fit[[k]]$cvm[which.min(fit[[k]]$cvm)],alpha[k],fit[[k]]$lambda[which.min(fit[[k]]$cvm)]))
                                    }
                                    colnames(optParam) <- c("BinomialDeviance","alpha","lambdaOpt")
                                    bestModel <-which.min(optParam[,1])
                                    .self$model <- fit[[bestModel]]
                                    .self$model$optParam <- optParam
                                  }
                                  
                                },
                                
                                customPredict = function(featureData){
                                  predictedResponse <- predict(.self$model, featureData, s="lambda.min")
                                  return(predictedResponse)
                                },
                                
                                getCoefficients = function(){
                                  return(coef(.self$model,s = "lambda.min"))
                                }
                                
                                )
                              
                              )
