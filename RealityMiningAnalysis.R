##Source all the prequisite scripts in order to make the analysis work
#Util.R is a script of helper functions useful in most of the other scripts in the project
#RealityMiningSetup.R cleans the data (originally in Matlab matrix format) for usage
#Main.R runs the deep neural network model used in the analysis
source("Util.R")
#Make sure rmadjacency.mat is in the same folder as RealityMiningSetup.R. Also need to install package "R.matlab".
source("RealityMiningSetup.R") 

hiddenCounts <- c(5)
W.init <- train.dbn(hiddenCounts,data,epochs=20)
rate <- 0.05; training.iters <- 3
#Autoencoder is an unsupervised variant of deep neural networks where the target output is the input itself.
#it is essentially "reconstructing" the input, and is useful for dimension reduction when extracting the middle 
#"code" layer. 
mode <- "autoencoder"
outlayernum <- ifelse(mode=="classification",2,dim(train.data[2])); layerCounts <- c(hiddenCounts,outlayernum)
W.start <- append.dbn.init(W.init,layerCounts)
W.s <- train.nn(data,layerCounts,W.start,rate,training.iters,mode=mode,verbose=TRUE)

##Extract the code layer - Innermost hidden layer. Serves as a dimension reduction preprocessing of the data.
code.length <- hiddenCounts[length(hiddenCounts)]
codes <- generate.codes(data,W.init,code.length)


##Cluster using K Means with K = 2. 
#D refers to clustering results using original data. 
#C refers to clustering results after dimension reduction, via extracting the code layer from a deep autoencoder. 
D <- kmeans(data,2)$cluster-1; C <- kmeans(codes,2)$cluster-1

##Boxplot of Sparsity
sparsity <- apply(data,1,mean); hist(sparsity)
C.ones <- sparsity[which(C==1)]; C.zeros <- sparsity[which(C==0)]
D.ones<- sparsity[which(D==1)]; D.zeros <- sparsity[which(D==0)]
boxplot(C.ones,C.zeros,D.ones,D.zeros,names=c("DL 1","DL 2","KM 1","KM 2"),
        main="Clusters when K=2")

##Histogram of Sparsity
hist(sparsity)
abline(v=min(C.ones),col=4); abline(v=max(C.zeros),col=4,lty=2)
abline(v=min(D.ones),col=2); abline(v=max(D.zeros),col=2,lty=2)


##Cluster using K Means w/ K = 3

#Setup
D3 <- kmeans(data,3)$cluster-1; C3 <- kmeans(codes,3)$cluster-1
D.0 <- data[which(D3==0),]; D.1 <- data[which(D3==1),]; D.2 <- data[which(D3==2),]
C.0 <- data[which(C3==0),]; C.1 <- data[which(C3==1),]; C.2 <- data[which(C3==2),]

#Look at sample images of the three different clusters
where <- 4:6
par(mfrow=c(3,3))
for(i in where) image(matrix(D.0[i,],92,92))
for(i in where) image(matrix(D.1[i,],92,92))
for(i in where) image(matrix(D.2[i,],92,92))

#Boxplot
par(mfrow=c(1,1))
D.zeros.3 <- sparsity[which(D3==0)]; D.ones.3 <- sparsity[which(D3==1)]
D.twos.3 <- sparsity[which(D3==2)]; C.zeros.3 <- sparsity[which(C3==0)]
C.ones.3 <- sparsity[which(C3==1)]; C.twos.3 <- sparsity[which(C3==2)]
boxplot(C.ones.3,C.zeros.3,C.twos.3,D.ones.3,D.zeros.3,D.twos.3,
        names=c("C1","C2","C3","D1","D2","D3"),main="Clusters when K=3")