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

Locally <- function(dataset, target, bandwidth){ 
	return(((ginv( (t(cbind(1,as.matrix(dataset[1:12]))) %*% weightLocally(dataset, target, bandwidth )) %*% cbind(1,as.matrix(dataset[1:12]))) %*% t(cbind(1,as.matrix(dataset[1:12])))) %*% weightLocally(dataset,target, bandwidth)) %*% dataset[,13])
}
#revisar


Ascent <- function(dataset, theta, alpha) {
  Data <- cbind(1,as.matrix(dataset[1:6]))
  for (i in 1:length(Data[,1]) ){	
  	gradient <-  (Data[i,] *  ((1/(1 + exp(-1*sum(Data[i,] * theta)))) - dataset[i,7]))
  	theta <- theta - alpha * gradient 
  }
  return(theta)
}

LinealNewtonRaphson  <- function(dataset){
	beta <-  ginv(t(cbind(1,as.matrix(dataset[1:12]))) %*% cbind(1 ,as.matrix(dataset[1:12]))) %*% t(cbind(1 ,as.matrix(dataset[1:12]))) %*% dataset[,13]
	return(beta)
}


LogisticNewtonRaphson <-function(dataset, beta){
	adjust <-  (cbind(1,as.matrix(dataset[1:6])) %*% beta)  + ginv(weightNewton(dataset, beta)) %*% (dataset[,7] - logistic( (cbind(1,as.matrix(dataset[1:6])) %*% beta )))
	return(beta + ( ginv( t(cbind(1,as.matrix(dataset[1:6]))) %*% weightNewton(dataset, beta) %*% cbind(1,as.matrix(dataset[1:6])), tol = sqrt(.Machine$double.eps)) %*% t(cbind(1,as.matrix(dataset[1:6]))) %*% weightNewton(dataset, beta) %*% adjust))
}

weightNewton <- function(dataset, beta){
	Data <- cbind(1,dataset[1:6])
	weight <- c()
	for(i in 1:length( Data[,1] )) {
		weight[i] <-  logistic(sum(Data[i,] * beta)) * (1 - logistic(sum(Data[i,] * beta)))
	}
	return(diag(weight, length(weight), length(weight)))	
}

weightLocally <- function(dataset, target, bandwidth){ ### Calcula los pesos
	Data <- cbind(1,dataset[1:12])
	weight <- c()
	for(i in 1:length( Data[,1] )) {
		weight[i] <-  exp (  -1 * (  sum( (Data[i,] - target)  * (Data[i,] - target ) / (2 * bandwidth^2) ) ) )
	}
	return(diag(weight, length(weight), length(weight)))	
}

logistic <- function(number){
	return(1/(1+exp(-number)))
}





