######### Algoritmos Pregunta 1 Tarea #################

Batch <- function(dataset, beta, alpha){
	delta <- t(cbind(1,as.matrix(dataset[1:12]))) %*% (dataset[,13] - (cbind(1,as.matrix(dataset[1:12])) %*% beta)) 
	return(beta - alpha * delta)
}

Stochastic  <- function(dataset, theta, alpha){
	Data <- cbind(1,dataset[1:12]) 
	for(i in 1:length(Data[1,])) {
		theta <- theta + alpha * ( Data[i,] * (dataset[i,13] - sum(Data[i,] * theta) ))
	}
	return(theta)
}
Ascent <- function(dataset, beta, alpha){
	Data <- cbind(1,dataset[1:12]) 
	for(i in 1:length(Data[1,])) {
		theta <- theta + alpha * ( Data[i,] * (dataset[i,13] - 1/(1 + e ^(-sum(Data[i,] * theta))) ))
	}
	return(theta)
}

Locally <- function(dataset, bandwidth){ 
	return(((solve((t(cbind(1,as.matrix(dataset[1:12]))) %*% weight(dataset, bandwidth )) %*% cbind(1,as.matrix(dataset[1:12]))) %*% t(cbind(1,as.matrix(dataset[1:12])))) %*% weight(dataset, bandwidth)) %*% dataset[,13])
}

weight <- function(dataset, bandwidth){ ### Calcula los pesos
	Data <- cbind(1,dataset[1:12])
	weight <- c()
	for(i in 1:length( Data[,1] )) {
		weight[i] <-  exp ((  -1 * (  sum((Data[i,] - dataset[,13])  * (Data[i,] - dataset[,13]) ) / 2 * bandwidth^2 ) ))
	}
	return(diag(weight, length(weight), length(weight)))	
}






