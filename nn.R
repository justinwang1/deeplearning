#Trains a neural network using backpropagation. Works for any number of hidden layers. Written for binary classification.
#Note: the paramter mode can be set to either "classification" or "autoencoder". If the latter, it trains a deep autoencoder,
#a nonsupervised variant of deep neural networks whose output is the input itself. In other words, an autoencoder "reconstructs"
#the input. It is useful for its middle code layer which can be used for nonlinear dimension reduction.
train.nn <- function(data,layerCounts,W,rate=0.05,training.iters=20,
				mode="classification",labels=NULL,verbose=FALSE)
{
  #Helper Functions
  sigm <- function(y) {
    return(1/(1+exp(-y)))
  }
  
  output.err <- function(target,output) {
    output*(1-output)*(target-output)
  }
  
  hidden.err <- function(outputs,prev.error,layernum) {
    #Input=0, Hidden 1=1, ..., Output=(numhiddenout)
    stopifnot(layernum >= 1 && layernum <= length(layerCounts))
    
    H <- outputs[[layernum]] 
    numinlayer <- layerCounts[layernum]
    weights <- W[[layernum+1]]
    prod <- (apply(weights,1,function(inp) 
    { return(sum(inp*prev.error)) }))[-1]
    
    err <- H*(1-H)*prod; err
  }
  
	#Main
	n <- dim(data)[1]; A <- training.iters
	for(a in 1:A)	
  {
  		#If verbose (default), print progress each iteration
  		if(verbose)	{
  			cat("Iteration ",a,"\n")
  		}
  
  	for(i in 1:n)	
    {
  		#Feedforward
  		numhiddenout <- length(layerCounts)
  		input <- data[i,]
  		
  		if(mode == "autoencoder") {
  			target <- input
  		} 
  		else if(mode == "classification") {
  			target <- c(0.9,0.1)
  			if(!labels[i])  target <- rev(target) 
  		}
  		else {
  			stop("Invalid Mode.")
  		}
  
  		outputs <- feed(input,W,"all"); output <- outputs[[numhiddenout]]
  
  		#Backpass	
  		errors <- list()		
  		errors[[1]] <- output.err(target,output)
  
  		for(k in 1:(numhiddenout-1)) {
  			errors[[k+1]] <- hidden.err(outputs,errors[[k]],numhiddenout-k)
  		}
  
  		errors <- rev(errors)
  
  		#Updates hidden weights
  		in.values <- list(); in.values[[1]] <- input
  		for(h in 1:(numhiddenout-1)) {
  			in.values[[h+1]] <- outputs[[h]]
  		}
  
  		for(s in numhiddenout:1) {	
  			valsPrev <- c(1,in.values[[s]])
  			errorsCurr <- errors[[s]]
  			lenCurr <- length(errorsCurr)
  			W.delta <- sweep(replicate(lenCurr,valsPrev),MARGIN=2,errorsCurr,`*`)
  			W[[s]] <- W[[s]] + rate*W.delta
  		}
  	}
	}
	
	return(W)
}