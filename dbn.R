#Trains a Deep Belief Network model in order to pretrain the initial weights for a deep neural network. In practice,
#the resulting initial weights trained by DBN are usually in the ballpark already of the local minimum.

train.dbn <- function(hiddenCounts,data,epochs=10) { 
  #Convert the data to batch data
  batchdata <- makebatches(data)
  
  #How many epochs to train each?
  numtrain <- length(hiddenCounts)
  myepochs <- rep(0,numtrain)
  if(length(epochs) > 1) {
      myepochs <- epochs 
  } else  { 
      myepochs <- rep(epochs,numtrain) 
  }
  
  #Run the first RBM
  resultsList <- list()
  results <- rbm(batchdata,numhid=hiddenCounts[1],maxepoch=myepochs[1])
  resultsList[[1]] <- results
  hidden <- results[[4]]
  
  #Run the additional RBMs
  if(!is.na(hiddenCounts[2])) {
    for(i in 2:numtrain) {
      results <- rbm(hidden,numhid=hiddenCounts[i],maxepoch=myepochs[i])
      resultsList[[i]] <- results
      hidden <- results[[4]]
    }
  }
  
  #Bind the results into a list
  myResults <- bind(resultsList); myResults
}  

bind <- function(results) {
  W.init <- list()
  num <- length(results)
  
  #Into code layer
  for(i in 1:num) {
    curr.res <- results[[i]] 
    W.init[[i]] <- rbind(curr.res[[2]],curr.res[[1]])
  }
  
  #out of code layer
  k <- num + 1
  for(j in num:1) {
    curr.res <- results[[j]]
    W.init[[k]] <- rbind(curr.res[[3]],t(curr.res[[1]]))
    k <- k+1
  }
  
  W.init
}

makebatches <- function(data) {
  totnum <- dim(data)[1]
  
  randomorder <- 1:totnum
  
  numbatches <- totnum/100
  numdims <- dim(data)[2]
  batchsize <- 100
  batchdata <- zeros(batchsize, numdims, numbatches)
  
  for(b in 1:numbatches) {
    batchdata[ , ,b] <- data[randomorder[(1+(b-1)*batchsize):(b*batchsize)], ]
  }
  batchdata 
}