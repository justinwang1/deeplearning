##Set seed so results can be compared for various parameter tweaks
set.seed(123456)

##Source other files to set up the model
source("rbm.R") #Function to train a Restricted Boltzmann Machine
source("dbn.R") #Function to train a Deep Belief Network
source("Util.R") #Utility function consisting of helper functions
source("nn.R") #Function to train a Neural Network w/ any number of hidden layers
source("MNIST.R") #Sets up the MNIST data. Make sure the MNIST data files are in the same folder as the
#file MNIST.R. 

##Create the data. We are taking a subset of the 0's and 1's only in this script to perform binary classification.
#1st par: Train sample size, 2nd par: test sample size, 3rd par: which digits to use?
train.size <- 5000; test.size <- 1000; digits <- c(0,1)
mysamp <- sample.mnist(train.size,test.size,digitstosample=digits) 
train.data <- mysamp$train.data; train.labels <- mysamp$train.labels
test.data <- mysamp$test.data; test.labels <- mysamp$test.labels


##Pretrain the weights using Deep Belief Network. Adjust the number of hidden layers and nodes in each hidden layer as 
#needed. For example, the default, hiddenCounts <- c(250,50,25), creates 3 hidden layers, with 250, 50, and 25 nodes
#respectively. 
#Note: If training deep autoencoder (unsupervised learning), make sure that the hidden layer counts are symmetric.
#Ex. hiddenCounts <- c(250,50,25,50,250)
hiddenCounts <- c(200,50,25); epochs <- 20 #Set the parameters for training Deep Belief Network
W.init <- train.dbn(hiddenCounts,train.data,epochs=epochs)


##Train a deep neural network using the pretrained weights from above as the initial weights. In practice, the initial
#weights are usually "most of the way there" already, making this step more akin to fine-tuning. If the weights were
#randomly initialized rather than pretrained, then the training algorithm (backpropagation) would "get lost" and be
#unable to find the local minimum needed. 

#Training takes about 5 minutes with default parameters
rate <- 0.05; iters <- 5 

#The two possible modes are "classification" and "autoencoder". The latter is used in RealityMiningAnalysis.R.
mode <- "classification"
outlayernum <- ifelse(mode=="classification",2,dim(train.data[2])); layerCounts <- c(hiddenCounts,outlayernum)
W.start <- append.dbn.init(W.init,layerCounts)
W.s <- train.nn(train.data,layerCounts,W.start,rate,iters,mode=mode,train.labels,verbose=TRUE)


##Get test error for MNIST digit dataset 1's and 0's
num <- dim(test.data)[1]
labels.hat <- rep(-1,num)
for(i in 1:num) {
  res <- feed(test.data[i,],W.s)
  labels.hat[i] <- ifelse(res[1] > res[2],1,0)
}
test.error <- sum(abs(labels.hat-test.labels))/num; test.error