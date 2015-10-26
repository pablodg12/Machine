#batch descendent algorithm#

batch <- function(dataset, theta, alpha){
	return(theta + alpha * colSums(cbind(1,dataset[1:12]) * (dataset[,13] - colSums(t(cbind(1,dataset[1:12]))*theta))))
}



Lineal <- function(dataset, theta, alpha){
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
		last = (length(dataset) * (i/partition))
	}
}

 prediction <- function(data, beta){
 	print(colSums(t(cbind(1,data[1:12])) * beta))
 	return((data[,13] - colSums(t(cbind(1,data[1:12])) * beta))^2)
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
