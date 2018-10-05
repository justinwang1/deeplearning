###READ TRAINING DATA

#Read the training data
to.read = file("train-images.idx3-ubyte", "rb")
readBin(to.read, integer(), n=4, endian="big")

#Parameters
num <- 60000
mnist.data <- matrix(0,num,28*28)

#Read in the data
for(i in 1:60000)
{
  Next <- matrix(readBin(to.read,integer(), size=1, n=28*28, endian="big"),28,28)
  Next <- Next[,28:1]
  mnist.data[i,] <- Next
}

#Fix error
mnist.data <- ifelse(mnist.data < 0,mnist.data+256,mnist.data)

#Scale
mnist.data <- mnist.data/255

#Close Read
close(to.read)

###READ THE LABELS
to.read = file("train-labels.idx1-ubyte", "rb")
readBin(to.read, 'integer', n=1, size=4, endian="big")
n <- readBin(to.read,'integer',n=1,size=4,endian='big')
mnist.labels <- readBin(to.read,'integer',n=n,size=1,signed=F)
close(to.read)
