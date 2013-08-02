binarization<-function(response,method = c("GMM","MEAN","MEDIAN","MEAN_SD","MEDIAN_MAD","ThirtySixty")){
  
  myGMM<-function(Y){  
    require(mixtools)
    a<-which(!is.na(Y))
    wait1 <- normalmixEM(as.numeric(Y[a]), lambda = .5, mu = c(-1,1), sigma = sd(Y[a]),maxit=100000)
    Y1<-matrix(NA,nrow=1,ncol = length(Y))
    Y1[a]<- 1*  (wait1$posterior[,1] <= wait1$posterior[,2])
    return(factor(Y1))
  }    
  
  myMEAN<-function(Y){
    Y <-factor(as.numeric(Y>= mean(Y,na.rm = T)))
    return(Y)
  }
  
  myMEAN_sd1<-function(Y){    
    low <- Y < (mean(Y,na.rm = T)-0.5*sd(Y,na.rm = T))
    high <- Y >= (mean(Y,na.rm = T)+0.5*sd(Y,na.rm = T))
    pos <- low|high
    threshold = mean(Y,na.rm = T)
    Y[low]<-0
    Y[high]<-1
    Y[-which(pos)]<-NA
    Y <-factor(Y)
    return(Y)      
  }
  myMEDIAN<-function(Y){
    Y <-factor(as.numeric(Y>= median(Y,na.rm = T)))
    return(Y)
  }
  
  myMEDIAN_mad1<-function(Y){          
    low <- Y <= (median(Y,na.rm = T)-0.5*mad(Y,na.rm = T))
    high <- Y >= (median(Y,na.rm = T)+0.5*mad(Y,na.rm = T))
    pos <- low|high
    Y[low]<-0
    Y[high]<-1
    Y[-which(pos)]<-NA
    Y <- factor(Y)
    return(Y)
  }
  myThirtySixty<-function(Y){
    Y<-as.numeric(Y)
    Y1<-Y[!is.na(Y)]
    
    X<-sort(Y1,index.return = T)
    a<-length(X$x)
    b1<-X$x[round(a/3)]
    b2<-X$x[round(a*2/3)]      
    low <- Y <= b1
    high <- Y >= b2
    pos <- low|high
    Y[low]<-0
    Y[high]<-1
    Y[-which(pos)]<-NA
    Y <- factor(Y)
    return(Y)
  }
  
  threshold.fun <- match.arg(method)
  switch(threshold.fun, 
         MEAN = (myfun = myMEAN),
         MEDIAN = (myfun = myMEDIAN),
         MEAN_SD = (myfun = myMEAN_sd1),
         MEDIAN_MAD = (myfun = myMEDIAN_mad1),
         GMM = (myfun = myGMM),
         ThirtySixty = (myfun = myThirtySixty))
  
  return(myfun(responseData))
}