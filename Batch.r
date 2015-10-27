#batch descendent algorithm#

batch <- function(dataset, beta, alpha){

	return (beta + alpha * t(cbind(1,as.matrix(dataset[1:12]))) %*% (dataset[,13] - (cbind(1,as.matrix(dataset[1:12])) %*% beta) ))

}

stochastic  <- function(dataset, theta, alpha){
	Data <- cbind(1,dataset[1:12]) 
	for(i in 1:length(Data)[,1]){
		theta <- theta + alpha * (Data[i,] *(dataset[,13] - sum(data[i,] *theta))
	}
	return(theta)
}

crossfolder <- function(dataset, partition){
	last = 1
	for(i in 1:partition){
		filename = paste("cross", i, ".data" , sep="")
		write.table(dataset[ last : (length(dataset[,1]) * (i/partition)), 1:13], filename, row.names = FALSE, col.names = FALSE)
		last <- (length(dataset[,1]) * (i/partition) + 1)
	}
}

 prediction <- function(data, beta){
 	return(cbind(1,as.matrix(data[1:12])) %*% beta )
 }


standardization <- function(dataset){
	data <- dataset

	for(i in 1:length(dataset[,1])) {
		for(k in 1:length(dataset[1,])) { 
			data[i,k] <- ( dataset[i,k] - min(dataset[,k]) ) / ( max(dataset[,k]) - min(dataset[,k]) )
		}
	}
	return(data)
}

normalization <-function(dataset){
	data <- dataset
	for(i in 1:length(dataset[,1])) {
		for(k in 1:length(dataset[1,])) { 
			data[i,k] <- ( dataset[i,k] - mean(dataset[,k]) ) / ( sd(dataset[,k]))
		}
	}
	
	return(data)
}





