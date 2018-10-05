#Trains a Restricted Boltzmann Machine. While it can be used by itself as a generative model, it is a building
#block here for training Deep Belief Networks. It can be mathematically shown that DBNs can be interpreted as a 
#stack of RBMs. 

rbm <- function(batchdata,numhid,maxepoch=10)
{
  #Set parameters
  restart <- 1; weightcost <- 0.0002
  epsilonw <- 0.1; epsilonvb <- 0.1; epsilonhb <- 0.1
  initialmomentum <- 0.5; finalmomentum <- 0.9
  
  numcases <- dim(batchdata)[1]; numdims <- dim(batchdata)[2]; numbatches <- dim(batchdata)[3]
  
  if(restart == 1) {
    restart <- 0
    epoch <- 1
  }
  
  #Initialize the variables
  vishid <- 0.1*randn(numdims,numhid); hidbiases <- zeros(1,numhid); visbiases <- zeros(1,numdims)
  poshidprobs <- zeros(numcases,numhid); neghidprobs <- zeros(numcases,numhid)
  vishidinc <- zeros(numdims,numhid); hidbiasinc <- zeros(1,numhid); visbiasinc <- zeros(1,numdims)
  batchposhidprobs <- zeros(numcases,numhid,numbatches)
  
  #Loop over epochs
  for(epoch in epoch:maxepoch) {
    errsum <- 0
    
    for(batch in 1:numbatches) {
      
      #Positive phase
      data <- batchdata[ , ,batch]
      poshidprobs <- 1/(1 + exp(-data %*% vishid - repmat(hidbiases,numcases)))
      batchposhidprobs[ , ,batch] <- poshidprobs
      posprods <- t(data) %*% poshidprobs
      poshidact <- sumM(poshidprobs); posvisact <- sumM(data)
      
      poshidstates <- (poshidprobs > rand(numcases,numhid))*1 
      
      #Negative phase
      negdata <- 1/(1 + exp(-poshidstates %*% t(vishid) - repmat(visbiases,numcases)))
      neghidprobs <- 1/(1 + exp(-negdata %*% vishid - repmat(hidbiases,numcases)))
      negprods <- t(negdata) %*% neghidprobs
      neghidact <- sumM(neghidprobs); negvisact <- sumM(negdata)
      
      err <- sum((data-negdata)^2)
      errsum <- err + errsum
      
      if(epoch > 5)
        momentum <- finalmomentum
      else
        momentum <- initialmomentum
      
      #Update weights and biases
      vishidinc <- momentum*vishidinc + 
        epsilonw*( (posprods - negprods)/numcases - weightcost*vishid)
      visbiasinc <- momentum*visbiasinc + (epsilonvb/numcases)*(posvisact - negvisact)
      hidbiasinc <- momentum*hidbiasinc + (epsilonhb/numcases)*(poshidact - neghidact)
      
      vishid <- vishid + vishidinc
      visbiases <- visbiases + visbiasinc
      hidbiases <- hidbiases + hidbiasinc
    }
    
    cat("Epoch ",epoch,"    Epoch Error: "," ",errsum,"\n")
  }
  
  list(vishid,hidbiases,visbiases,batchposhidprobs)
}

zeros <- function(a,b,c=NA) {
  if(is.na(c)) {
    array(rep(0,a*b),c(a,b))
  } else {
    array(rep(0,a*b*c),c(a,b,c)) 
  }
}

randn <- function(a,b){ 
  matrix(rnorm(a*b),a,b) 
}

rand <- function(a,b) {
  matrix(runif(a*b),a,b)
}

repmat <- function(A,n) {
  stopifnot(dim(A)[1] == 1)
  At <- as.vector(t(A))
  return(t(replicate(n,At)))
}

sumM <- function(A) {
  t(apply(A,2,sum))
}