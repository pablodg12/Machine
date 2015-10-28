#batch descendent algorithm#

batch <- function(dataset, beta, alpha){
	delta <- t(cbind(1,as.matrix(dataset[1:12]))) %*% (dataset[,13] - (cbind(1,as.matrix(dataset[1:12])) %*% beta)) 
	return(beta - alpha * delta)
}

stochastic  <- function(dataset, theta, alpha){
	Data <- cbind(1,dataset[1:12]) 
	for(i in 1:length(Data[1,])) {
		theta <- theta + alpha * ( Data[i,] * (dataset[i,13] - sum(Data[i,] * theta) ))
	}
	return(theta)
}

ascent <- function(dataset, beta, alpha){
	Data <- cbind(1,dataset[1:12]) 
	for(i in 1:length(Data[1,])) {
		theta <- theta + alpha * ( Data[i,] * (dataset[i,13] - 1/(1 + e ^(-sum(Data[i,] * theta))) ))
	}
	return(theta)
}








