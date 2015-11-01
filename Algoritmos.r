######### Tarea de Machine Learning #################

Batch <- function(dataset, beta, alpha){
	delta <- t(cbind(1,as.matrix(dataset[1:12]))) %*% (dataset[,13] - (cbind(1,as.matrix(dataset[1:12])) %*% beta)) 
	return(beta + alpha * delta)
}

Stochastic  <- function(dataset, theta, alpha){
	Data <- cbind(1,dataset[1:12]) 
	for(i in 1:length(Data[,1])) {
		theta <- theta + alpha * ( Data[i,] * (dataset[i,13] - sum(Data[i,] * theta) ))
	}
	return(theta)
}

Locally <- function(dataset, bandwidth){ 
	return(((solve( (t(cbind(1,as.matrix(dataset[1:12]))) %*% weightLocally(dataset, bandwidth )) %*% cbind(1,as.matrix(dataset[1:12]))) %*% t(cbind(1,as.matrix(dataset[1:12])))) %*% weightLocally(dataset, bandwidth)) %*% dataset[,13])
}
#revisar#
Ascent <- function(dataset, beta, alpha){
	for(i in 1:length(dataset[1,])) {
		beta <- beta + alpha * ( cbind(1,dataset[1:6])[i,] * (dataset[i,7] - logistic(sum(cbind(1,dataset[1:6])[i,] * beta)) ))
	}
	return(beta)
}

NewtonRaphson <-function(dataset, beta){

	adjust <- dataset[,13] - logistic( (cbind(1,as.matrix(dataset[1:12])) %*% beta ))
	return(beta + ( ginv( t(cbind(1,as.matrix(dataset[1:12]))) %*% weightNewton(dataset, beta) %*% cbind(1,as.matrix(dataset[1:12])), tol = sqrt(.Machine$double.eps)) %*% t(cbind(1,as.matrix(dataset[1:12]))) %*% adjust))

}

weightNewton <- function(dataset, beta){
	Data <- cbind(1,dataset[1:(length(dataset)-1)])
	weight <- c()
	for(i in 1:length( Data[,1] )) {
		weight[i] <-  logistic(sum(Data[i,] * beta)) * (1 - logistic(sum(Data[i,] * beta)))
	}
	return(diag(weight, length(weight), length(weight)))	
}

weightLocally <- function(dataset, bandwidth){ ### Calcula los pesos
	Data <- cbind(1,dataset[1:12])
	weight <- c()
	for(i in 1:length( Data[,1] )) {
		weight[i] <-  exp ((  -1 * (  sum((Data[i,] - dataset[,13])  * (Data[i,] - dataset[,13]) ) / 2 * bandwidth^2 ) ))
	}
	return(diag(weight, length(weight), length(weight)))	
}

logistic <- function(number){
	return(exp(number)/(1+exp(number)))
}





