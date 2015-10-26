batch <- function(dataset, theta, alpha){
	return(theta + alpha * colSums(cbind(1,dataset[1:12]) * (dataset[,13] - colSums(t(cbind(1,dataset[1:12]))*theta))))
}



Lineal <- function(dataset, theta, alpha){
	Data <- cbind(1,dataset[1:12]) 
	for(i in 1:length(Data)){
		theta <- theta + alpha * (Data[i,] *(dataset[,13] - sum(data[i,] *theta))
	}
	return(theta)
}


 prediction <- function(data, beta){
 	return(sum(cbind(1,data[1:12]) * beta))
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
