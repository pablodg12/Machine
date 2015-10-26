
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