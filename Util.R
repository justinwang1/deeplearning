#Various utility functions used in other files.

feed <- function(input,W,mode="last") {
  numhiddenout <- length(W)
  Z <- c(1,input)
  store <- list()
  
  for(i in 1:numhiddenout)
  {
    Wi <- W[[i]]
    Z <- c(1,sigm(t(Wi) %*% Z))
    store[[i]] <- Z[-1]
  }
  Z <- Z[-1]
  
  if(mode=="all")   return(store)
  else if(mode=="code") return(store[length(store)/2][[1]])
  else   return(Z)
}


feed.linear <- function(input,W,mode="last") {
  numhiddenout <- length(W)
  Z <- c(input,1)
  store <- list()
  
  for(i in 1:numhiddenout)
  {
    Wi <- W[[i]]
    
    if(i != numhiddenout/2)
      Z <- c(sigm(t(Wi) %*% Z),1)
    else
      Z <- c(t(Wi) %*% Z,1)
    store[[i]] <- Z[-length(Z)]
    
  }
  Z <- Z[-length(Z)]
  
  if(mode=="all")   return(store)
  else if(mode=="code") return(store[length(store)/2][[1]])
  else   return(Z)
}


generate.codes <- function(data,weights,code.length) {
  num.data <- dim(data)[1]
  
  codes <- matrix(0,num.data,code.length)
  for(i in 1:num.data)
  {
    codes[i,] <- feed(data[i,],weights,mode="code")
    if(i %% 100 == 0) cat("Iteration: ",i,"\n")
  }
  codes
}

sample.mnist <- function(train.size,test.size,digitstosample=c(0,1)) {
  allcandidatedigits <- which(mnist.labels == digitstosample[1] | mnist.labels == digitstosample[2])
  mysubset <- sample(allcandidatedigits,train.size+test.size)
  trainsubset <- mysubset[1:train.size]; testsubset <- mysubset[(train.size+1):t(train.size+test.size)]
  
  train.data <- mnist.data[trainsubset,]
  train.labels <- mnist.labels[trainsubset]; train.labels <- ifelse(train.labels == digitstosample[2],1,0)
  test.data <- mnist.data[testsubset,]
  test.labels <- mnist.labels[testsubset]; test.labels <- ifelse(test.labels == digitstosample[2],1,0)
  list(train.data=train.data,train.labels=train.labels,test.data=test.data,test.labels=test.labels)
}


sigm <- function(y) {
  return(1/(1+exp(-y)))
}


append.dbn.init <- function(W.init,layerCounts) {
  len <- length(layerCounts)-1
  d1 <- dim(W.init[[len]])[2]+1; d2 <- layerCounts[length(layerCounts)]
  W.classlayer <- matrix(runif(d1*d2,-0.05,0.05),d1,d2)
  W.start <- W.init[1:len]; W.start[[len+1]] <- W.classlayer
  W.start
}


randInit.classification <- function(layerCounts) {
  W <- list()
  
  for(i in 1:(length(layerCounts)-1))
  {
    numrows <- layerCounts[i]+1
    W[[i]] <- matrix(runif((layerCounts[i]+1)*
                             layerCounts[i+1],-0.05,0.05),nrow=numrows)
  }
  W
}


test.classification <- function(test.data,test.labels,W) {
  stopifnot(dim(test.data)[1]==length(test.labels))
  len <- length(test.labels)
  testresults <- rep(0,len)
  
  for(i in 1:len)
  {
    curr <- test.data[i,]
    result <- feed(curr,W)
    if(result[1] > result[2]) 
      testresults[i] <- 1
  }
  class.error <- mean(abs(testresults - test.labels)); class.error
}