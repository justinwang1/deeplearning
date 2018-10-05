##READ AND SETUP THE DATA
#dates: A matrix, where the ith row records every date (in Date form) corresponding to a 
#call event for person i. 
#contacts - Similar, but records a contact number. Recorded as -1 if contact is not in the study.
library(R.matlab)
reality <- readMat("rmadjacency.mat")
dates <- reality[[1]]; contacts <- reality[[2]]


##Figure out which subjects need to be removed at the very end (they didn't call anybody)
numnotzero <- apply(dates,1,function(x) { sum(x != 0)})
threshold <- 1; remove <- which(numnotzero < threshold)
len <- dim(dates)[1]


##CLEAN THE DATE AND CONTACT LISTS
#NOTE: Both of these lists remove all the 0's at the end from the original date and contacts. Contacts not
#in study were also removed. 
#dates.char: Same as date, but in character format instead of Date format. 
#datesNum.list: Assigns a graph/observation number to each date. 
#contacts.list: same as contacts, but removing the 0's at the end. 
Mdate <- function(val) as.Date(val - 1, origin = '0000-01-01') 
Days <- as.character(seq(as.Date("2004-01-19"),as.Date("2005-07-15"),"days"))
contacts.list <- list(); datesNum.list <- list()

for(i in 1:len)
{
  keep <- which(contacts[i,] != 0 & contacts[i,] != -1)
  contacts.list[[i]] <- contacts[i,keep]
  datesNum.list[[i]] <- match(as.character(Mdate(dates[i,]))[keep],Days)
}


##CONVERT THE ABOVE DATA TO THE GRAPH FORM DESIRED
n <- len; days <- length(Days)
graph.data <- array(rep(0,n*n*days),c(n,n,days))

for(i in 1:len)
{
  #numEvents = Number of total call events triggered by person i 
  numEvents <- length(datesNum.list[[i]])
  if(numEvents == 0) next 
  
  #Only go through the contacts in network
  for(j in 1:numEvents)
  {
    person <- i; friend <- contacts.list[[i]][j]
    network.num <- datesNum.list[[i]][j]
    
    #Some people were given IDs outside the scope of the experiment, so don't count them
    if(friend > len) next
    
    graph.data[person,friend,network.num] <- 1; graph.data[friend,person,network.num] <- 1 
  }
}

##CLEAN THE DATA
graph.data <- graph.data[-remove,-remove,]
num.graphs <- dim(graph.data)[3]
empty.graphs <- rep(0,num.graphs); m <- 1

for(k in 1:num.graphs)
{
  if(sum(graph.data[,,k]) == 0) {
    empty.graphs[m] <- k
    m <- m + 1
  }
}

empty.graphs <- empty.graphs[-which(empty.graphs==0)]
graph.data <- graph.data[,,-empty.graphs]


##CONVERT DATA INTO 2D 
new.len <- dim(graph.data)[1]; new.num <- dim(graph.data)[3]
data <- matrix(0,new.num,new.len^2)
for(i in 1:new.num)
  data[i,] <- c(graph.data[,,i])
