# Deep Learning and Applications to Network Data
An artificial neural network (ANN) with any number of hidden layers can be trained successfully by pretraining the weights using Deep Belief Networks (Hinton 2006). Here we provide a deep ANN (ANNs with many hidden layers) implementation in R with the aforementioned pretraining methods. Two modes for usage are implemented - classification and autoencoder. The latter is an ANN variant used for reconstruction of the input, and is useful for dimension reduction as the lower dimensional middle "encoded" layer can be extracted. This implementation is tested on the MNIST digit dataset. Also included is an application to the network dataset Reality Mining, in which a deep autoencoder is used to preprocess the data before clustering, to superior results.  

## Description of code files
1. *Main.R.* The main file for running the deep ANN implementation on the MNIST digit dataset. Parameters and settings can be tuned within the main file, including the usage mode: classification or autoencoder. 

2. *nn.R.* An implementation of a neural network trained using backpropagation. Two modes are provided: classification and autoencoder, and any number of hidden layers can be used. For classification, currently only performs binary classification, but can be easily adapted for K categories, K >= 3. 

3. *dbn.R.* Implementation of Deep Belief Network, the model used to pretrain the weights of a deep ANN. If the weights are not pretrained, the ANN training will usually fail for 3 or more hidden layers. 

4. *rbm.R.* Implementation of Restricted Boltzmann Machine, an unsupervised model that forms the building block of a Deep Belief Network. In fact, training for a Deep Belief Network can be thought of as training a stack of Restricted Boltzmann Machines. 

5. *Util.R.* Provides various utility function used in other scripts. 

6. *MNIST.R.* Prepares the MNIST dataset for usage. 

7. *RealityMiningSetup.R.* Sets up the Reality Mining Dataset, a network dataset collected by Stanford researchers Nathan Eagle and Alex Pentland (2005). Using this dataset, we apply deep autoencoders to improve K means clustering results. 

8. *RealityMiningAnalysis.R.* Applies deep autoencoders to K means clustering for Reality Mining. 

## Description of data files

1. *Files ending in -ubyte.* The raw data files for the MNIST digit dataset that are processed for usage in MNIST.R. 

2. *rmadjacency.mat.* The Reality Mining dataset as a Matlab MAT-file object. Used in RealityMiningSetup.R to clean the data and convert it into something usable in R.  
